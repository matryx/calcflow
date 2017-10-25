using UnityEngine;
using System.Collections;
// Source: http://blogs.unity3d.com/2012/09/07/property-drawers-in-unity-4/ 

namespace VoxelBusters.Utility
{
	public class RegexAttribute : PropertyAttribute 
	{
		#region Properties

		public readonly string pattern;
		public readonly string helpMessage;
	
		#endregion

		#region Constructor

		public RegexAttribute (string pattern, string helpMessage) 
		{
			this.pattern = pattern;
			this.helpMessage = helpMessage;
		}

		#endregion
	}
}