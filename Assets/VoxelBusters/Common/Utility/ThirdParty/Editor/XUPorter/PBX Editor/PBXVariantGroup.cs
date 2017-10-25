using UnityEngine;
using System.Collections;
using System.Collections.Generic;

namespace VoxelBusters.ThirdParty.XUPorter
{
	public class PBXVariantGroup : PBXGroup
	{
		#region Constructor
		
		public PBXVariantGroup( string name, string path = null, string tree = "SOURCE_ROOT" ) 
			: base(name, path, tree)
		{
		}
		
		public PBXVariantGroup( string guid, PBXDictionary dictionary ) : base( guid, dictionary )
		{
		}
		
		#endregion
	}
}
