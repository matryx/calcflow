using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility
{
	public partial class ShaderUtility : AdvancedScriptableObject <ShaderUtility>
	{
		public enum eShaderPropertyType
		{
			COLOR,
			VECTOR,
			FLOAT,
			RANGE,
			TEXTURE
		}
	}
}