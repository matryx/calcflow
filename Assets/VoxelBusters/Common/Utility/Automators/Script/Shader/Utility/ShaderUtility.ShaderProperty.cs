using UnityEngine;
using System.Collections;
using System;

#if UNITY_EDITOR
using UnityEditor;
#endif
namespace VoxelBusters.Utility
{
	public partial class ShaderUtility : AdvancedScriptableObject <ShaderUtility>
	{
		[Serializable]
		public class ShaderProperty
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
			private			eShaderPropertyType			m_type;
			public			eShaderPropertyType 		Type
			{
				get
				{
					return m_type;
				}
				
				private set
				{
					m_type	= value;
				}
			}
			
			#endregion
			
			#region Constructor
			
#if UNITY_EDITOR
			public ShaderProperty (string _name, ShaderUtil.ShaderPropertyType _propertyType)
			{
				Name	= _name;
				
				switch (_propertyType)
				{
				case ShaderUtil.ShaderPropertyType.Color:
					Type	= eShaderPropertyType.COLOR;
					break;
					
				case ShaderUtil.ShaderPropertyType.Vector:
					Type	= eShaderPropertyType.VECTOR;
					break;
					
				case ShaderUtil.ShaderPropertyType.Float:
					Type	= eShaderPropertyType.FLOAT;
					break;
					
				case ShaderUtil.ShaderPropertyType.Range:
					Type	= eShaderPropertyType.RANGE;
					break;
					
				case ShaderUtil.ShaderPropertyType.TexEnv:
					Type	= eShaderPropertyType.TEXTURE;
					break;
					
				default:
					throw new Exception("[ShaderUtility] Unknown type.");
				}
			}
#endif
			
			#endregion
		}
	}
}