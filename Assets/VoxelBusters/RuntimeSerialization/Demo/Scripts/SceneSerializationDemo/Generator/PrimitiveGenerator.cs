using UnityEngine;
using System.Collections;
using System.Collections.Generic;

namespace VoxelBusters.RuntimeSerialization.Demo
{
	[RuntimeSerializable(typeof(MonoBehaviour), false)]
	public class PrimitiveGenerator : MonoBehaviour 
	{
		#region Constants

		private		const		string					kSerializationIDForPrefs	= "primitive-generator-prefs";
		private		const		string					kSerializationIDForFiles	= "primitive-generator-files";

		#endregion

		#region Properties

		[SerializeField, RuntimeSerializeField]
		private					List<GameObject>		m_prefabs;

		[RuntimeSerializeField]
		private 				List<GameObject>		m_spawnedGameObjectList		= new List<GameObject>();

		#endregion

		#region Methods

		public void SpawnNewObject ()
		{
			if (m_spawnedGameObjectList == null)
				m_spawnedGameObjectList			= new List<GameObject>();

			// Instantiate
			int 		_randomIndex			= Random.Range(0, m_prefabs.Count);
			GameObject 	_newCloneGO				= RSUtility.Instantiate(m_prefabs[_randomIndex]);

			// Create clone in random position with random rotation
			Vector3		_randPosition			= Random.insideUnitSphere * 5f;
			_randPosition.y						= _randPosition.y * 0.5f + 4.5f;

			Transform 	_newCloneTransform		= _newCloneGO.transform;
			_newCloneTransform.parent			= transform;
			_newCloneTransform.localPosition	= _randPosition;
			_newCloneTransform.localRotation	=  Random.rotationUniform;

			// Add it to the list					
			m_spawnedGameObjectList.Add(_newCloneGO);
		}

		public void Serialize ()
		{
			// For WebPlayer, we are saving data to player prefs. 
			// Note: We are using unique identifiers for player prefs and file based saving.
#if (UNITY_WEBPLAYER || UNITY_WEBGL)		
			RSManager.Serialize<PrimitiveGenerator>(this, kSerializationIDForPrefs, eSaveTarget.PLAYER_PREFS);
#else
			RSManager.Serialize<PrimitiveGenerator>(this, kSerializationIDForFiles, eSaveTarget.FILE_SYSTEM);
#endif
		}

		public void Deserialize ()
		{
			Clear();

			// For WebPlayer, we are reading data from player prefs. 
			// Note: We are using unique identifiers for player prefs and file based saving.
#if (UNITY_WEBPLAYER || UNITY_WEBGL)		
			RSManager.Deserialize<PrimitiveGenerator>(kSerializationIDForPrefs);
#else
			RSManager.Deserialize<PrimitiveGenerator>(kSerializationIDForFiles);
#endif
		}

		public void Clear ()
		{
			if (m_spawnedGameObjectList == null)
				return;

			foreach (GameObject _spawnedGO in m_spawnedGameObjectList)
				DestroyImmediate(_spawnedGO);

			m_spawnedGameObjectList.Clear();
		}

		#endregion
	}
}