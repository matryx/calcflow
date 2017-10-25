using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility
{
	public class ExecuteOnValueChangeAttribute : PropertyAttribute 
	{
		#region Properties
		
		public string InvokeMethod 
		{ 
			get; 
			private set; 
		}
		
		#endregion

		#region Constructor

		private ExecuteOnValueChangeAttribute ()
		{}

		public ExecuteOnValueChangeAttribute (string _method)
		{
			InvokeMethod	= _method;
		}

		#endregion
	}
}