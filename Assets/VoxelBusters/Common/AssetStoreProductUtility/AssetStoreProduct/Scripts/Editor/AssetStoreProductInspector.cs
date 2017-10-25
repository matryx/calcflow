using UnityEngine;
using UnityEditor;
using System.Collections;
using VoxelBusters.Utility;

namespace VoxelBusters.AssetStoreProductUtility
{
	using Internal;

	public class AssetStoreProductInspector : AdvancedScriptableObjectInspector
	{
		#region GUI Methods

		protected override void OnGUIWindow ()
		{
			AssetStoreProduct _product	= (target as IAssetStoreProduct).AssetStoreProduct;
			
			if (_product == null || _product.LogoTexture == null)
				return;

			// GUI style
			GUIStyle 	_guiStyle		= new GUIStyle("label");
			_guiStyle.richText			= true;
			_guiStyle.wordWrap			= true;

			GUILayout.BeginHorizontal();
			{
				// Logo
				GUILayout.BeginVertical();
				{
					GUILayout.Space(10f);
					GUILayout.Label(_product.LogoTexture);
				}
				GUILayout.EndVertical();

				// Product details and copyrights
				GUILayout.BeginVertical();
				{
					string _pName		= "<size=30>" + _product.ProductName + "</size>";
					string _pVersion	= "<size=12>Version " + _product.ProductVersion + "</size>";
					string _pCopyRights	= "<i><size=10>" + Constants.kCopyrights + "</size></i>";

					GUILayout.Label(_pName, _guiStyle, GUILayout.Height(38f));
					GUILayout.Label(_pVersion, _guiStyle);
					GUILayout.Label(_pCopyRights, _guiStyle);
				}
				GUILayout.EndVertical();
				GUILayout.FlexibleSpace();
			}
			GUILayout.EndHorizontal();
		}

		#endregion
	}
}