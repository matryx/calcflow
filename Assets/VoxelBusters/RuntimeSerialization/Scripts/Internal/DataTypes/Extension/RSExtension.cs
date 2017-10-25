using UnityEngine;
using System.Collections;
using System;
using System.Reflection;

namespace VoxelBusters.RuntimeSerialization.Internal
{
	public struct RSExtension
	{
		#region Static Fields

		private		static 		Assembly	extensionAssembly	= null;

		#endregion

		#region Fields
		
		private					Type		m_objectType;
		private					Type		m_type;
		private					bool		m_isExtensionInstanceCreated;
		private					object		m_instance;

		#endregion

		#region Properties

		public Type ObjectType
		{	
			get
			{
				return m_objectType;
			}
		}
		
		public Type Type
		{
			get
			{
				return m_type;
			}
		}
		
		public object Instance
		{
			get
			{
				if (!m_isExtensionInstanceCreated)
				{
					m_isExtensionInstanceCreated	= true;
					m_instance						= Activator.CreateInstance(m_type);
				}
				
				return m_instance;
			}
		}
		
		#endregion
		
		#region Constructor
		
		public RSExtension(Type _objectType, bool _createInstance) : this(_objectType, GetExtensionType(_objectType), _createInstance)
		{}
		
		public RSExtension(Type _objectType, Type _extensionType, bool _createInstance) : this()
		{
			if (_objectType == null)
				throw new ArgumentNullException("[RS] Object type is null.");

			// Initialize
			m_objectType						= _objectType;
			m_type								= _extensionType;
			
			if (_extensionType == null)
			{
				m_isExtensionInstanceCreated	= true;
				m_instance						= null;
			}
			else
			{
				m_isExtensionInstanceCreated	= _createInstance;
				
				if (_createInstance)
					m_instance					= Activator.CreateInstance(_extensionType);
			}
		}

		public RSExtension(Type _objectType, object _extension) : this()
		{
			if (_objectType == null)
				throw new ArgumentNullException("[RS] Object type is null.");

			// Initialize
			m_objectType						= _objectType;
			m_instance							= _extension;
			m_type								= (_extension == null) ? null : _extension.GetType();
			m_isExtensionInstanceCreated		= true;
		}
		
		#endregion
		
		#region Static Methods
		
		private static Type GetExtensionType(Type _objectType)
		{
			bool		_isGenericType		= _objectType.IsGenericType;

			if (extensionAssembly == null)
				extensionAssembly			= typeof(RSExtension).Assembly;

			// Check if extension class is implemented using type's full name
			string		_extensionFullName	= GetExtensionTypeName(_objectType.FullName, 	_isGenericType);
			Type 		_extensionType		= extensionAssembly.GetType(_extensionFullName,	false);
			
			// Check if extension class is implemented using type's name
			if (_extensionType == null)
			{
				string	_extensionName		= GetExtensionTypeName(_objectType.Name, 		_isGenericType);
				_extensionType				= extensionAssembly.GetType(_extensionName, 	false);
			}

			return _extensionType;
		}
		
		private static string GetExtensionTypeName(string _name, bool _isGenericType)
		{
			if (_name == null)
				throw new ArgumentException("[RS] Name cant be null.");
			
			if (_isGenericType)
				return _name.Substring(0, _name.IndexOf('`')) + Constants.kExtensionSupportSuffix;

			return _name + Constants.kExtensionSupportSuffix;
		}
		
		#endregion
	}
}