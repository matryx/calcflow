using UnityEngine;
using System.Collections;
using System;
using System.Collections.Generic;
using VoxelBusters.Utility;

namespace VoxelBusters.RuntimeSerialization
{
	using Internal;

	/// <summary>
	/// Stores all the data required to serialize or deserialize an object.
	/// </summary>
	public class RuntimeSerializationInfo
	{
		#region Properties

		public Type ObjectType
		{
			get;
			private set;
		}

		internal Dictionary<string, RuntimeSerializationEntry> InitializerValuesCollection
		{
			get;
			private set;
		}

		internal Dictionary<string, RuntimeSerializationEntry> NonInitializerValuesCollection	
		{
			get;
			private set;
		}

		public int MemberCount
		{
			get
			{
				return InitializerValuesCollection.Count + NonInitializerValuesCollection.Count;
			}
		}

		#endregion

		#region Constructors

		private RuntimeSerializationInfo ()
		{}

		internal RuntimeSerializationInfo (Type _targetType)
		{
			if (_targetType == null)
				throw new NullReferenceException("Target object type cant be null.");
			
			// Set properties
			ObjectType						= _targetType;
			InitializerValuesCollection		= new Dictionary<string, RuntimeSerializationEntry>();
			NonInitializerValuesCollection	= new Dictionary<string, RuntimeSerializationEntry>();
		}

		internal RuntimeSerializationInfo (Type _targetType, Dictionary<string, RuntimeSerializationEntry> _initializerValuesCollection, Dictionary<string, RuntimeSerializationEntry> _nonInitializerValuesCollection)
		{
			if (_targetType == null)
				throw new NullReferenceException("Target object type cant be null.");

			// Set properties
			ObjectType						= _targetType;
			InitializerValuesCollection		= _initializerValuesCollection;
			NonInitializerValuesCollection	= _nonInitializerValuesCollection;
		}

		#endregion

		#region Add Value Methods

		/// <summary>
		/// Adds the specified object into the <see cref="RuntimeSerializationInfo"/> for serialization, where it is associated with name.
		/// </summary>
		/// <param name="_name">The name associated with the value to be stored in <see cref="RuntimeSerializationInfo"/>.</param>
		/// <param name="_value">The value to be serialized. Any children of this object will automatically be serialized.</param>
		/// <param name="_isObjectInitializer">The flag indicates whether this value is object intializer. Object initializers are the values that are available while creating object instance.</param>
		/// <typeparam name="T">The Type associated with the current object. This must always be the type of the object itself.</typeparam>
		public void AddValue <T> (string _name, T _value, bool _isObjectInitializer = false)
		{
			AddValue(_name, _value, typeof(T), _isObjectInitializer);
		}

		/// <summary>
		/// Adds the specified object into the <see cref="RuntimeSerializationInfo"/> for serialization, where it is associated with name.
		/// </summary>
		/// <param name="_name">The name associated with the value to be stored in <see cref="RuntimeSerializationInfo"/>.</param>
		/// <param name="_value">The value to be serialized. Any children of this object will automatically be serialized.</param>
		/// <param name="_valueType">The Type associated with the current object. This must always be the type of the object itself.</param>
		/// <param name="_isObjectInitializer">The flag indicates whether this value is object intializer. Object initializers are the values that are available while creating object instance.</param>
		public void AddValue (string _name, object _value, Type _valueType, bool _isObjectInitializer = false)
		{	
			RuntimeSerializationEntry	_newEntry	= new RuntimeSerializationEntry(_name, _value, _valueType);

			if (_isObjectInitializer)
				InitializerValuesCollection.Add(_name, _newEntry);
			else
				NonInitializerValuesCollection.Add(_name, _newEntry);
		}

		#endregion

		#region Get Value Methods

		/// <summary>
		/// Gets the value from <see cref="RuntimeSerializationInfo"/> using specified name.
		/// </summary>
		/// <returns>The object of specified type associated with name.</returns>
		/// <param name="_name">The name associated with the value to be retreived from <see cref="RuntimeSerializationInfo"/>.</param>
		/// <param name="_isObjectInitializer">The flag indicates whether this value is object intializer. Object initializers are the values that are available while creating object instance.</param>
		/// <typeparam name="T">The Type of the value to retrieve.</typeparam>
		public T GetValue <T> (string _name, bool _isObjectInitializer = false)
		{
			return (T)GetValue(_name, typeof(T), _isObjectInitializer);
		}

		/// <summary>
		/// Gets the value from <see cref="RuntimeSerializationInfo"/> using specified name.
		/// </summary>
		/// <returns>The object of specified type associated with name.</returns>
		/// <param name="_name">The name associated with the value to be retreived from <see cref="RuntimeSerializationInfo"/>.</param>
		/// <param name=="_type">The Type of the value to retrieve.</param>
		/// <param name="_isObjectInitializer">The flag indicates whether this value is object intializer. Object initializers are the values that are available while creating object instance.</param>
		public object GetValue (string _name, Type _type, bool _isObjectInitializer = false)
		{
			object	_value;

			TryGetValue(_name, out _value, _type, _isObjectInitializer);

			return _value;
		}

		/// <summary>
		/// Gets the value from <see cref="RuntimeSerializationInfo"/> using specified name.
		/// </summary>
		/// <returns><c>true</c>, if value was found in <see cref="RuntimeSerializationInfo"/>, <c>false</c> otherwise.</returns>
		/// <param name="_name">The name associated with the value to be retreived from <see cref="RuntimeSerializationInfo"/>.</param>
		/// <param name="_value">The value associated with the specified name.</param>
		/// <param name="_isObjectInitializer">The flag indicates whether this value is object intializer. Object initializers are the values that are available while creating object instance.</param>
		/// <typeparam name="T">The Type of the value to retrieve.</typeparam>
		public bool TryGetValue <T> (string _name, out T _value, bool _isObjectInitializer = false)
		{
			object	_serializedValue;
			bool	_success	= TryGetValue(_name, out _serializedValue, typeof(T), _isObjectInitializer);

			// Update reference value
			_value				= (T)_serializedValue;

			return _success;
		}

		/// <summary>
		/// Gets the value from <see cref="RuntimeSerializationInfo"/> using specified name.
		/// </summary>
		/// <returns><c>true</c>, if value was found in <see cref="RuntimeSerializationInfo"/>, <c>false</c> otherwise.</returns>
		/// <param name="_name">The name associated with the value to be fetched from <see cref="RuntimeSerializationInfo"/>.</param>
		/// <param name="_value">The value associated with the specified name.</param>
		/// <param name=="_type">The Type of the value to be fetched.</param>
		/// <param name="_isObjectInitializer">The flag indicates whether this value is object intializer. Object initializers are the values that are available while creating object instance.</param>
		public bool TryGetValue (string _name, out object _value, Type _type, bool _isObjectInitializer = false)
		{
			// Get appropriate serialized values container
			Dictionary<string, RuntimeSerializationEntry> _valuesCollection	= null;
			
			if (_isObjectInitializer)
				_valuesCollection	= InitializerValuesCollection;
			else
				_valuesCollection	= NonInitializerValuesCollection;
			
			// Fetch value associated with given name and check target values validity
			RuntimeSerializationEntry _entry;
			
			if (_valuesCollection.TryGetValue(_name, out _entry))
			{
				if (_entry.Value != null && _type.IsInstanceOfType(_entry.Value))	
					_value	= _entry.Value;
				else
					_value	= _type.DefaultValue();
				
				return true;
			}

			// Requested value not found
			_value			= _type.DefaultValue();

			return false;
		}

		/// <summary>
		/// Determines whether the <see cref="RuntimeSerializationInfo"/> contains a specific value.
		/// </summary>
		/// <returns><c>true</c>, if value with specified name exists in <see cref="RuntimeSerializationInfo"/>, <c>false</c> otherwise.</returns>
		/// <param name="_name">The name associated with the value to be fetched from <see cref="RuntimeSerializationInfo"/>.</param>
		/// <param name="_isObjectInitializer">The flag indicates whether this value is object intializer. Object initializers are the values that are available while creating object instance.</param>
		public bool ContainsValue (string _name, bool _isObjectInitializer = false)
		{
			Dictionary<string, RuntimeSerializationEntry> _valuesCollection	= null;
			
			if (_isObjectInitializer)
				_valuesCollection	= InitializerValuesCollection;
			else
				_valuesCollection	= NonInitializerValuesCollection;

			return _valuesCollection.ContainsKey(_name);
		}

		#endregion
	}
}