using UnityEngine;
using System.Collections;
using VoxelBusters.AssetStoreProductUtility.Demo;

namespace VoxelBusters.RuntimeSerialization.Demo
{
	public class SceneSerializationDemo : DemoSubMenu 
	{
		#region Methods

		protected override void OnEnable ()
		{
			base.OnEnable ();

			// Load scene
			Application.LoadLevel("SceneSerializationDemo");
		}

		protected override void OnGUIWindow ()
		{}

		#endregion
	}
}