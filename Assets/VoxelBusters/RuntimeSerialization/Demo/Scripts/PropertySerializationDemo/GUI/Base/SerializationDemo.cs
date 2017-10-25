using UnityEngine;
using System.Collections;
using VoxelBusters.AssetStoreProductUtility.Demo;

namespace VoxelBusters.RuntimeSerialization.Demo
{
	public abstract class SerializationDemo : DemoSubMenu, IRuntimeSerializationEventListener 
	{
		#region Properties

		public				SerializationSample			SerializationTarget
		{
			get;
			set;
		}

		public abstract 	string						Save2PrefsSerializationID
		{
			get;
		}

		public abstract 	string						Save2FileSerializationID
		{
			get;
		}

		private				string						SerializationData;

		#endregion

		#region Unity Methods

		protected override void Awake ()
		{
			base.Awake ();

			// Create new sample
			SerializationTarget		= CreateNewSampleInstance();
		}

		protected override void OnEnable ()
		{
			base.OnEnable ();

			// Show current state of our target
			if (SerializationTarget != null)
				AddNewResult(SerializationTarget.ToString());

			// Register for serialization callback (if required)
			RSManager.RegisterEventListener(Save2PrefsSerializationID, 		this as IRuntimeSerializationEventListener);
			RSManager.RegisterEventListener(Save2FileSerializationID,		this as IRuntimeSerializationEventListener);
		}

		protected override void OnDisable ()
		{
			base.OnDisable ();
			
			// Unregister serialization callback
			RSManager.UnRegisterEventListener(Save2PrefsSerializationID,	this as IRuntimeSerializationEventListener);
			RSManager.UnRegisterEventListener(Save2FileSerializationID,		this as IRuntimeSerializationEventListener);
		}

		#endregion

		#region Abstract Method

		protected abstract SerializationSample CreateNewSampleInstance ();

		#endregion 
		
		#region API Calls

		private void SerializeWithNoSave ()
		{
			// Note: We havent registered for callbacks, so once serialization is completed we are updating status of it
			SerializationData	= RSManager.Serialize<SerializationSample>(SerializationTarget);

			// Updating serialization status
			AddNewResult("Serialization finished");
			AppendResult(SerializationData);
		}
		
		private void DeserializeWithNoSave ()
		{
			// Note: We havent registered for callbacks, so once deserialization is completed we are updating status of it
			SerializationTarget	= RSManager.DeserializeData<SerializationSample>(SerializationData);
			
			// Updating deserialization status
			AddNewResult("Deserialization finished");
			
			// Print deserialized value
			PrintObject(SerializationTarget, false);
		}

		private void SerializeToPlayerPrefs ()
		{
			RSManager.Serialize<SerializationSample>(SerializationTarget, Save2PrefsSerializationID,	eSaveTarget.PLAYER_PREFS);
		}

		private void DeserializeFromPlayerPrefs ()
		{
			// As 2nd parameter "_targetObject" isnt specified, object of new instance is created on deserialization
			// If "_targetObject" was specified then deserialized value would be assigned to the target instance
			SerializationTarget = RSManager.Deserialize<SerializationSample>(Save2PrefsSerializationID);
		}

#if !(UNITY_WEBPLAYER || UNITY_WEBGL)
		private void SerializeToFile ()
		{
			RSManager.Serialize<SerializationSample>(SerializationTarget, Save2FileSerializationID, 	eSaveTarget.FILE_SYSTEM);
		}

		private void DeserializeFromFile ()
		{
			// As 2nd parameter "_targetObject" isnt specified, object of new instance is created on deserialization
			// If "_targetObject" was specified then deserialized value would be assigned to the target instance
			SerializationTarget = RSManager.Deserialize<SerializationSample>(Save2FileSerializationID);
		}
#endif

		private void GetSerializationDataFromPlayerPrefs ()
		{
			string _serializationData	= RSManager.GetSerializationData(Save2PrefsSerializationID);

			if (string.IsNullOrEmpty(_serializationData))
				AddNewResult("Serialization data: NULL");
			else
				AddNewResult("Serialization data: " + _serializationData);
		}

		private void GetSerializationDataFromFile ()
		{
			string _serializationData	= RSManager.GetSerializationData(Save2FileSerializationID);
			
			if (string.IsNullOrEmpty(_serializationData))
				AddNewResult("Serialization data: NULL");
			else
				AddNewResult("Serialization data: " + _serializationData);
		}

		private void SaveSerializationData ()
		{
			RSManager.Save();
		}

		#endregion

		#region API Callbacks
		
		public void OnAfterRuntimeSerialize (string _key, object _object)
		{
			AddNewResult("Received object serialization finished event.");
			AppendResult(string.Format("Key used for serialization is {0}.", _key));
			
			// Print serialized value
			PrintObject(_object, false);
		}

		public void OnAfterRuntimeDeserialize (string _key, object _object)
		{
			AddNewResult("Received object deserialization finished event.");
			AppendResult(string.Format("Key used for deserialization is {0}.", _key));

			// Print deserialized value
			PrintObject(_object, false);
		}

		#endregion
		
		#region UI
		
		protected override void OnGUIWindow()
		{		
			base.OnGUIWindow();
			
			RootScrollView.BeginScrollView();
			{
				// Start vertical column
				GUILayout.BeginVertical(UISkin.scrollView);
				{		
					if (GUILayout.Button("Create New Instance (Demo Purpose)"))
					{
						SerializationTarget		= CreateNewSampleInstance();
					}

					if (GUILayout.Button("Assign Random Value (Demo Purpose)"))
					{
						// Assign random value
						if (SerializationTarget != null)
							SerializationTarget.AssignRandomValue();

						PrintObject(SerializationTarget, true);
					}
					
					if (GUILayout.Button("Print Object Value"))
					{
						PrintObject(SerializationTarget, true);
					}


					GUILayout.Label("Save Target : None",kSubTitleStyle);
					GUILayout.BeginHorizontal();
					{
						if (GUILayout.Button("Serialize"))
						{
							SerializeWithNoSave();
						}
						
						if (GUILayout.Button("Deserialize"))
						{
							DeserializeWithNoSave();
						}
					}
					GUILayout.EndHorizontal();

						
					GUILayout.Label("Save Target : Player Preferences",kSubTitleStyle);
					GUILayout.BeginHorizontal();
					{
						if (GUILayout.Button("Serialize"))
						{
							SerializeToPlayerPrefs();
						}
	
						if (GUILayout.Button("Deserialize"))
						{
							DeserializeFromPlayerPrefs();
						}
					}
					GUILayout.EndHorizontal();

#if !(UNITY_WEBPLAYER || UNITY_WEBGL)

					GUILayout.Label("Save Target : File",kSubTitleStyle);
					GUILayout.BeginHorizontal();
					{
						if (GUILayout.Button("Serialize"))
						{
							SerializeToFile();
						}
	
						if (GUILayout.Button("Deserialize"))
						{
							DeserializeFromFile();
						}
					}
					GUILayout.EndHorizontal();
#else

					GUI.enabled = false;
					GUILayout.Label("Save Target : File [Disabled - File access off on Web]",kSubTitleStyle);					
					GUI.enabled = true;

#endif
	
					GUILayout.Label("Misc",kSubTitleStyle);
					GUILayout.BeginVertical();
					{
						if (GUILayout.Button("Get Serialization Data From Player Preferences"))
						{
							GetSerializationDataFromPlayerPrefs();
						}
					
#if !(UNITY_WEBPLAYER || UNITY_WEBGL)
						if (GUILayout.Button("Get Serialization Data From File"))
						{
							GetSerializationDataFromFile();
						}
#endif
	
						if (GUILayout.Button("Save"))
						{
							SaveSerializationData();
						}
					}
					GUILayout.EndVertical();
				}
				GUILayout.EndVertical();
			}
			RootScrollView.EndScrollView();
			
			DrawResults();
			
			DrawPopButton();
		}
		
		#endregion

		#region Misc Methods

		private void PrintObject (object _object, bool _newResult)
		{
			if (_newResult)
			{
				if (_object == null)
					AddNewResult("Object value is NULL\n");
				else
					AddNewResult("Object value\n" + _object.ToString());
			}
			else
			{
				if (_object == null)
					AppendResult("Object value is NULL\n");
				else
					AppendResult("Object value\n" + _object.ToString());
			}
		}

		#endregion
	}
}
