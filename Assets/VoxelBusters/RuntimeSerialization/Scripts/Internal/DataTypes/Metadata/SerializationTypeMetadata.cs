using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using System;
using VoxelBusters.Utility;
using System.Reflection;

namespace VoxelBusters.RuntimeSerialization.Internal
{
	internal class SerializationTypeMetadata
	{
		#region Constants
		
		private 	const 	UInt32 									kCachedObjectTypeIDOffset		= 64;
		
		#endregion
		
		#region Fields
		
		// Related to assembly caching and referencing
		private				UInt32									m_assemblyCounter;
		private 			Dictionary<Assembly, UInt32> 			m_cachedAssemblies;
		
		// Related to type caching and referencing
		private				UInt32									m_typeCounter;
		private 			Dictionary<Type, UInt32> 				m_cachedObjectTypes;
		
		#endregion
		
		#region Constuctors
		
		internal SerializationTypeMetadata ()
		{
			// Related to assembly caching
			m_assemblyCounter				= 0;
			m_cachedAssemblies				= new Dictionary<Assembly, UInt32>();
			
			// Related to type caching
			m_typeCounter					= kCachedObjectTypeIDOffset;
			m_cachedObjectTypes				= new Dictionary<Type, UInt32>();
		}
		
		#endregion
		
		#region Methods
		
		internal eTypeTag GetTypeTag (Type _objectType)
		{
			eTypeTag _typeTag	= eTypeTag.UNSUPPORTED;
			
			if (_objectType.IsValueType)
			{
				if (SerializationTypeUtil.IsPrimitive(_objectType))
				{
					_typeTag	= eTypeTag.PRIMITIVE;
				}
				else if (_objectType.IsEnum)
				{
					_typeTag	= eTypeTag.ENUM;
				}
				else if (SerializationTypeUtil.IsRuntimeSerializableObject(_objectType))
				{
					_typeTag	= eTypeTag.STRUCT;
				}
			}
			else if (_objectType == typeof(string))
			{
				_typeTag		= eTypeTag.STRING;
			}
			else if (_objectType.IsArray)
			{
				int _rank		= _objectType.GetArrayRank();
				
				if (_rank == 1 || _rank == 2)
				{
					_typeTag	= eTypeTag.ARRAY;
				}
			}
			else if (_objectType.IsClass)
			{
				if (SerializationTypeUtil.IsRuntimeSerializableObject(_objectType))
				{
					_typeTag	= eTypeTag.CLASS;
				}
			}
			
			return _typeTag;
		}
		
		internal bool IsPrimitive (UInt32 _objectTypeID)
		{
			if (_objectTypeID < kCachedObjectTypeIDOffset)
			{
				TypeCode	_typeCode		= (TypeCode)_objectTypeID;
				
				return (_typeCode != TypeCode.String);
			}
			
			return false;
		}
		
		internal void WriteTypeMetadata (RSBinaryWriter _binaryWriter, Type _type, UInt32 _typeID)
		{
			if (_type == null)
				throw new NullReferenceException("[RS] Serialize type metadata failed. Type is null.");
			
			// Get assembly info
			bool 		_newAssembly;
			UInt32 		_assemblyID;
			Assembly 	_assembly		= _type.Assembly;
			
			RegisterAssembly(_assembly, out _assemblyID, out _newAssembly);
			
			// Write assembly properties (if required)
			if (_newAssembly)
			{
				_binaryWriter.WriteBinaryElement(BinaryElement.ASSEMBLY);
				_binaryWriter.Write(_assemblyID);
				_binaryWriter.Write(_assembly.GetName().Name);
			}
			
			// Write type properties
			_binaryWriter.WriteBinaryElement(BinaryElement.TYPE);
			_binaryWriter.Write(_assemblyID);
			_binaryWriter.Write(_typeID);
			_binaryWriter.Write(_type.FullName);
		}
		
		internal void ReadTypeMetaData (RSBinaryReader _binaryReader, BinaryElement _binaryElement)
		{
			// Read assembly info
			if (_binaryElement == BinaryElement.ASSEMBLY)
			{
				UInt32 		_assemblyID			= _binaryReader.ReadUInt32();
				string 		_assemblyName		= ReadAssemblyName(_binaryReader);
				Assembly 	_assembly			= Assembly.Load(_assemblyName);
				
				if (_assembly == null)
					throw new Exception(string.Format("[RS] Couldnt load assembly with name={0}.", _assemblyName));
				
				// Add assembly info to cache
				m_cachedAssemblies.Add(_assembly, _assemblyID);
				
				// Read next element
				_binaryElement					= _binaryReader.ReadBinaryElement();
			}
			
			// Read type info
			if (_binaryElement == BinaryElement.TYPE)
			{
				UInt32 		_assemblyID			= _binaryReader.ReadUInt32();
				UInt32 		_typeID				= _binaryReader.ReadUInt32();
				string 		_typeFullName		= _binaryReader.ReadString();
				Assembly 	_assembly			= GetAssembly(_assemblyID);
				
				if (_assembly == null)
					throw new Exception(string.Format("[RS] Assembly with ID={0} couldnt be found.", _assemblyID));
				
				Type 		_newType			= _assembly.GetType(_typeFullName, true);
				
				// Add type info to cache
				m_cachedObjectTypes.Add(_newType, _typeID);
			}
		}
		
		internal void Reset ()
		{
			// Related to assembly caching
			m_assemblyCounter				= 0;
			m_cachedAssemblies.Clear();
			
			// Related to type caching
			m_typeCounter					= kCachedObjectTypeIDOffset;
			m_cachedObjectTypes.Clear();
		}
		
		#endregion
		
		#region Assembly Methods
		
		private void RegisterAssembly (Assembly _assembly, out UInt32 _assemblyID, out bool _newAssembly)
		{
			// Check if this assembly exists in our cache
			if (m_cachedAssemblies.TryGetValue(_assembly, out _assemblyID))
			{
				_newAssembly	= false;
				return;
			}
			
			// Generate assembly identifier which is used later for mapping
			_newAssembly		= true;
			_assemblyID			= m_assemblyCounter++;
			
			// Add assembly info to cache
			m_cachedAssemblies.Add(_assembly, _assemblyID);
		}
		
		private Assembly GetAssembly (UInt32 _assemblyID)
		{
			// Reset enumerator
			Dictionary<Assembly, UInt32>.Enumerator	 _assemblyEnumerator	= m_cachedAssemblies.GetEnumerator();
			
			// Start iterating through enumerator entries 
			while (_assemblyEnumerator.MoveNext())
			{
				KeyValuePair<Assembly, UInt32>		_curKeyValuePair		= _assemblyEnumerator.Current;
				
				if (_curKeyValuePair.Value == _assemblyID)
				{
					return _curKeyValuePair.Key;
				}
			}
			
			return null;
		}
		
		#endregion
		
		#region Type Methods
		
		internal void RegisterType (Type _type, out UInt32 _typeID, out bool _newType)
		{
			// TypeCode is used as type id for primitive's and string's
			if (SerializationTypeUtil.IsPrimitive(_type) || _type == typeof(string))
			{
				_typeID		= (UInt32)Type.GetTypeCode(_type);
				_newType	= false;
				return;
			}
			
			// Check if type was already registered
			if (m_cachedObjectTypes.TryGetValue(_type, out _typeID))
			{
				_newType	= false;
				return;
			}
			
			// Generate type identifier which is used later for mapping
			_newType		= true;
			_typeID			= m_typeCounter++;
			
			// Add type info to cache
			m_cachedObjectTypes.Add(_type, _typeID);
		}
		
		internal Type GetType (UInt32 _typeID)
		{
			if (_typeID < kCachedObjectTypeIDOffset)
				return ((TypeCode)_typeID).GetTypeFromTypeCode();
			
			// Start iterating through enumerator entries 
			Dictionary<Type, UInt32>.Enumerator		_typesEnumerator	= m_cachedObjectTypes.GetEnumerator();
			
			while (_typesEnumerator.MoveNext())
			{
				KeyValuePair<Type, UInt32>			_curKeyValuePair	= _typesEnumerator.Current;
				
				if (_curKeyValuePair.Value == _typeID)
				{
					return _curKeyValuePair.Key;
				}
			}
			
			return null;
		}
		
		#endregion
		
		#region Misc. Methods
		
		private string ReadAssemblyName (RSBinaryReader _binaryReader)
		{
			string 	_assemblyName		= _binaryReader.ReadString();
			int		_seperatorIndex		= _assemblyName.IndexOf(',');
			
			if (_seperatorIndex != -1)
				return _assemblyName.Substring(0, _seperatorIndex);
			
			return _assemblyName;
		}
		
		#endregion
	}
}