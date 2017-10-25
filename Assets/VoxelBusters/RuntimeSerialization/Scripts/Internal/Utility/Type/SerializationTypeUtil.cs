using UnityEngine;
using System.Collections;
using System;
using System.Collections.Generic;
using System.Reflection;
using VoxelBusters.Utility;

namespace VoxelBusters.RuntimeSerialization.Internal
{
	internal class SerializationTypeUtil
	{
		#region Properties

		private 	static		Dictionary<Type, RuntimeSerializableAttribute>		serializableAttributeCache;
		private 	static 		Dictionary<Type, List<Field>>						typeMemberInfoCache;

		#endregion

		#region Constructor

		static SerializationTypeUtil ()
		{
			// Intialise
			serializableAttributeCache	= new Dictionary<Type, RuntimeSerializableAttribute>();
			typeMemberInfoCache			= new Dictionary<Type, List<Field>>();
		}

		#endregion

		#region RS Methods

		internal static void Initialise ()
		{
			// Adding system extensions
			RSExtensionManager.AddNewExtension(typeof(System.ValueType), null);
			RSExtensionManager.AddNewExtension(typeof(System.Object), null);
		}
	
		internal static void Purge (Type _type)
		{
			if (_type == null)
				throw new Exception("[RS] Type cant be null.");

			// Clear information cached for this type
			serializableAttributeCache.Remove(_type);
			typeMemberInfoCache.Remove(_type);
		}

		#endregion

		#region Type Methods
	
		internal static bool IsPrimitive (Type _type)
		{
			return (_type.IsPrimitive || _type == typeof(DateTime));
		}
		
		internal static bool IsRuntimeSerializableObject (Type _objectType)
		{
			// Check if type supports member based serialization
			RuntimeSerializableAttribute 	_serializableAttribute	= GetRuntimeSerializableAttribute(_objectType);
			
			if (_serializableAttribute != null)
				return true;

			// Check if type support Extension based serialization
			RSExtension 					_extension	= RSExtensionManager.GetExtension(_objectType);

			return (_extension.Type != null);
		}

		#endregion

		#region Serializable Attribute Methods

		internal static RuntimeSerializableAttribute GetRuntimeSerializableAttribute (Type _objectType)
		{	
			RuntimeSerializableAttribute _serializableAttribute;

			lock (serializableAttributeCache)
			{
				// If cached attribute doesnt exist then use reflection to get attribute info
				if (!serializableAttributeCache.TryGetValue(_objectType, out _serializableAttribute))
				{
					_serializableAttribute					= _objectType.GetAttribute<RuntimeSerializableAttribute>(false);
					serializableAttributeCache[_objectType]	= _serializableAttribute;
				}
			}

			return _serializableAttribute;
		}

		#endregion

		#region Fields Methods
		
		internal static List<Field> GetRuntimeSerializableFields (Type _objectType, RuntimeSerializableAttribute _runtimeSerializableAttribute)
		{	
			List<Field>		_serializableFieldList;

			lock (typeMemberInfoCache)
			{
				// If cached value doesnt exist, then use Reflection to get list of RuntimeSerializable fields
				if (typeMemberInfoCache.TryGetValue(_objectType, out _serializableFieldList))
					return _serializableFieldList;

				bool 		_serializeAllPublicFields 		= false;
				bool 		_serializeAllNonPublicFields	= false;
				
				if (_runtimeSerializableAttribute != null)
				{
					_serializeAllPublicFields				= _runtimeSerializableAttribute.SerializeAllPublicVariables;
					_serializeAllNonPublicFields			= _runtimeSerializableAttribute.SerializeAllNonPublicVariables;
				} 
				
				// Fetch info about public as well as non-public fields
				FieldInfo[] _publicFieldInfoList			= _objectType.GetFields(BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly | BindingFlags.Static);
				FieldInfo[] _nonPublicFieldInfoList			= _objectType.GetFields(BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.DeclaredOnly | BindingFlags.Static);
				
				// List holds both public and non-public fields which needs to be serialised
				_serializableFieldList						= new List<Field>(_publicFieldInfoList.Length + _nonPublicFieldInfoList.Length);
				
				if (_serializeAllPublicFields)
					RemoveNonSerializableFields(_publicFieldInfoList, ref _serializableFieldList);
				else
					AddSerializableFields(_publicFieldInfoList, ref _serializableFieldList);

				if (_serializeAllNonPublicFields)
					RemoveNonSerializableFields(_nonPublicFieldInfoList, ref _serializableFieldList);
				else
					AddSerializableFields(_nonPublicFieldInfoList, ref _serializableFieldList);
				
				// Cache this info
				typeMemberInfoCache[_objectType]			= _serializableFieldList;
			}

			return _serializableFieldList;
		}

		private static void RemoveNonSerializableFields (FieldInfo[] _fieldInfoList, ref List<Field> _serializableFieldsList)
		{
			int 	_count	= _fieldInfoList.Length;

			for (int _iter = 0; _iter < _count; _iter++)
			{
				FieldInfo	_currentFieldInfo	= _fieldInfoList[_iter];

				// Constants fields should be ignored
				if (_currentFieldInfo.IsLiteral)
					continue;

				// Consider attribute to decide if object is serializable or not
				NonRuntimeSerializedFieldAttribute 	_ignoreAttribute	= _currentFieldInfo.GetAttribute<NonRuntimeSerializedFieldAttribute>(false);

				if (_ignoreAttribute != null)
					continue;

				_serializableFieldsList.Add(new Field(_currentFieldInfo));
			}
		}

		private static void AddSerializableFields (FieldInfo[] _fieldInfoList, ref List<Field> _serializableFieldsList)
		{
			int 	_count	= _fieldInfoList.Length;
			
			for (int _iter = 0; _iter < _count; _iter++)
			{
				FieldInfo	_currentFieldInfo	= _fieldInfoList[_iter];

				// Constants fields should be ignored
				if (_currentFieldInfo.IsLiteral)
					continue;
				
				// Consider attribute to decide if object is serializable or not
				RuntimeSerializeFieldAttribute 	_serializeAttribute	= _currentFieldInfo.GetAttribute<RuntimeSerializeFieldAttribute>(false);

				if (_serializeAttribute == null)
					continue;

				_serializableFieldsList.Add(new Field(_currentFieldInfo, _serializeAttribute));
			}
		}

		#endregion
	}
}