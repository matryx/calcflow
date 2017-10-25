using UnityEngine;
using System.Collections;
using UnityEditor;
using VoxelBusters.AssetStoreProductUtility;

namespace VoxelBusters.RuntimeSerialization.Internal
{
	[InitializeOnLoad]
	public class ProductMenu 
	{
		#region Constants

		private 	const	string				kLogoPath			= "Assets/VoxelBusters/RuntimeSerialization/EditorResources/Logo/Logo.png";
		private		const	string				kMenuPath			= "Window/Voxel Busters/Runtime Serialization/";

		#endregion

		#region Properties

		private		static	AssetStoreProduct	AssetStoreProduct;

		#endregion

		#region Constructors

		static ProductMenu ()
		{
			// Unity callbacks
			EditorApplication.update	-= EditorUpdateCallback;
			EditorApplication.update	+= EditorUpdateCallback;
		}

		#endregion

		#region Static Methods

		private static void EditorUpdateCallback ()
		{
			// Unregister from callback
			EditorApplication.update	-= EditorUpdateCallback;

			// Create instance if required
			if (AssetStoreProduct == null)
			{
				AssetStoreProduct		= new AssetStoreProduct(Constants.kProductName, Constants.kProductVersion, kLogoPath);
			}
		}

		#endregion

		#region Menu Item Methods

		[MenuItem(kMenuPath + "Clear", false, 200)]
		private static void Clear ()
		{
			RSManager.RemoveAll();
			RSManager.Save();
		}

		[MenuItem(kMenuPath + "Check for Updates", false, 225)]
		private static void CheckForUpdatesNow ()
		{
			if (AssetStoreProduct != null)
				AssetStoreProduct.CheckForUpdates();
		}

		#endregion
	}
}