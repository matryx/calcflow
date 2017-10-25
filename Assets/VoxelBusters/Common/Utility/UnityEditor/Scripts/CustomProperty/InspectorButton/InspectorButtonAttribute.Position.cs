using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility
{
	public partial class InspectorButtonAttribute : PropertyAttribute 
	{
		public enum ePosition
		{
			TOP,
			BOTTOM
		}
	}
}