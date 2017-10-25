using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility
{
	public partial class InspectorButtonAttribute : PropertyAttribute 
	{
		#region Properties

		public		string			Name 
		{ 
			get; 
			private set; 
		}

		public		string			InvokeMethod 
		{ 
			get; 
			private set; 
		}

		public		ePosition		Position 
		{ 
			get; 
			private set; 
		}
		
		#endregion
		
		#region Constructor

		private	InspectorButtonAttribute ()
		{}
		
		public InspectorButtonAttribute (string _buttonName, string _invokeMethod, ePosition _position = ePosition.TOP)
		{
			Name			= _buttonName;
			InvokeMethod	= _invokeMethod;
			Position		= _position;
		}
		
		#endregion
	}
}