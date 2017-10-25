using UnityEngine;
using System.Collections;
using System;
using System.Collections.Generic;

#if UNITY_EDITOR
using UnityEditor;
#endif
namespace VoxelBusters.Utility
{
	public partial class ShaderUtility : AdvancedScriptableObject <ShaderUtility>
	{
		[Serializable]
		public class ShaderInfo 
		{
			#region Properties

			[SerializeField]
			private			string						m_name;
			public 			string						Name
			{
				get
				{
					return m_name;
				}
				
				private set
				{
					m_name	= value;
				}
			}

			[SerializeField]
			private			List<ShaderProperty>		m_propertyList;
			public 			List<ShaderProperty>		PropertyList
			{
				get
				{
					return m_propertyList;
				}
				
				private set
				{
					m_propertyList	= value;
				}
			}

			#endregion

			#region Constructors

			private ShaderInfo ()
			{}

#if UNITY_EDITOR
			public ShaderInfo (Shader _shader)
			{
				if (_shader == null)
					throw new Exception("[ShaderUtility] Couldnt find shader with name");

				// Initialize
				Name			= _shader.name;
				PropertyList	= new List<ShaderProperty>();
			
				// Iterate through properties
				int 	_propertyCount	= ShaderUtil.GetPropertyCount(_shader);

				for (int _iter = 0; _iter < _propertyCount; _iter++)
				{
					string							_propertyName	= ShaderUtil.GetPropertyName(_shader, _iter);
					ShaderUtil.ShaderPropertyType 	_propertyType	= ShaderUtil.GetPropertyType(_shader, _iter);
					ShaderProperty					_newProperty	= new ShaderProperty(_propertyName, _propertyType);

					// Add it to list
					PropertyList.Add(_newProperty);
				}
			}
#endif

			#endregion

			#region Methods

			public Shader GetShader ()
			{
				return Shader.Find(Name);
			}

			#endregion
		}
	}
}