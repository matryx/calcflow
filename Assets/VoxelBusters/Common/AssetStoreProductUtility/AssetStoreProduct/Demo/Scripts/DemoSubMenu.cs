using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using VoxelBusters.Utility;

namespace VoxelBusters.AssetStoreProductUtility.Demo
{
	public class DemoSubMenu : DemoGUIWindow 
	{
		#region Properties
	
		private List<string>			m_results			= new List<string>();
		
		//Misc
		private GUIScrollView 			m_resultsScrollView;
	
		#endregion
		
		#region Unity Methods
	
		protected override void Start()
		{
			base.Start();
	
			//For drawing results in scrollview
			m_resultsScrollView  =  gameObject.AddComponent<GUIScrollView>();
		}

		#endregion
	
		#region Virtual Methods
	
		protected virtual void DrawPopButton(string _popTitle = "Back")
		{	
			if(GUILayout.Button(_popTitle))
			{
				gameObject.SetActive(false);
			}	
		}
	
		protected override void OnGUIWindow ()
		{
			base.OnGUIWindow ();
			GUILayout.Box(name);
		}
		
		#endregion
	
	
		#region For Displaying and Tracking Results
	
		protected void AppendResult(string _result)
		{
			m_results.Add(_result);
		}
		
		protected void AddNewResult(string _result)
		{
			m_results.Clear();
			m_results.Add(_result);
		}
		
		protected void DrawResults()
		{
			GUILayout.FlexibleSpace();

			if(m_results.Count > 0)
			{
				m_resultsScrollView.BeginScrollView(UISkin.window,GUILayout.MinHeight(Screen.height*0.3f));
				{
					for(int _i = 0 ; _i < m_results.Count ; _i++ )
					{
						string _result  =  m_results[_i];
						
						if(_i == 0)
						{
							GUILayout.Box(_result);
						}
						else 
						{
							GUILayout.Label(_result);	
						}
						
					}
					GUILayout.FlexibleSpace();
				}
				m_resultsScrollView.EndScrollView();
			}
		}
	
		#endregion
	
	}
}