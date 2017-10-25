using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility
{
	public partial class AndroidManifestGenerator 
	{
		public struct Feature
		{
			#region Properties

			public			string			Name
			{
				get;
				private set;
			}
			
			public			bool			Required
			{
				get;
				private set;
			}
			
			#endregion

			#region Constructor
			
			public Feature (string _name, bool _required) : this ()
			{
				this.Name		= _name;
				this.Required	= _required;
			}

			#endregion
		}
	}
}