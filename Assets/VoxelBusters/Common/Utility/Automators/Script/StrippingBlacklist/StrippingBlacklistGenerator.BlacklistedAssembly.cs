using UnityEngine;
using System.Collections;
using System.Collections.Generic;

#if UNITY_EDITOR
using UnityEditor;

namespace VoxelBusters.Utility
{
	public partial class StrippingBlacklistGenerator : AssetPostprocessor
	{
		public class BlacklistedAssembly
		{
			#region Properties

			public					string				Name
			{
				get;
				private set;
			}

			public					List<string>		TypeList
			{
				get;
				private set;
			}

			public					List<string>		NamespaceList
			{
				get;
				private set;
			}

			#endregion

			#region Constructors

			private BlacklistedAssembly ()
			{}

			public BlacklistedAssembly (string _assemblyName)
			{
				Name			= _assemblyName;
				TypeList		= new List<string>();
				NamespaceList	= new List<string>();
			}

			#endregion

			#region Methods

			public void BlackListType (string _newType)
			{
				if (TypeList.Contains(_newType))
					return;

				TypeList.Add(_newType);
			}

			public void BlackListNamespace (string _newNamespace)
			{
				if (NamespaceList.Contains(_newNamespace))
					return;
				
				NamespaceList.Add(_newNamespace);
			}

			#endregion
		}
	}
}
#endif