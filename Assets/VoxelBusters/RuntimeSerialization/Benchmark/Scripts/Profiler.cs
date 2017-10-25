using UnityEngine;
using System.Collections;
using System;
using System.IO;
using System.Diagnostics;
using System.Runtime.Serialization.Formatters.Binary;
using System.Collections.Generic;
using VoxelBusters.RuntimeSerialization;
using VoxelBusters.Utility;

namespace VoxelBusters.RuntimeSerialization.Benchmark
{
	using Internal;

	[RuntimeSerializable]
	public class Profiler : MonoBehaviour 
	{
		#region Properties

		private enum TestCase
		{
			UNDEFINED,
			BS_SERIALIZATION,
			BS_DESERIALIZATION,
			RS_SERIALIZATION_CLASS,
			RS_DESERIALIZATION_CLASS,
			RS_SERIALIZATION_GAMEOBJECT,
			RS_DESERIALIZATION_GAMEOBJECT,
			RS_SERIALIZATION_PREFAB,
			RS_DESERIALIZATION_PREFAB
		}

		[SerializeField, RuntimeSerializeFieldAttribute] 
		private 			BenchmarkSample		m_benchmarkSample					= null;

		[SerializeField, RuntimeSerializeFieldAttribute] 
		private 			GameObject			m_serializeGameObject;

		[SerializeField, RuntimeSerializeFieldAttribute] 
		private 			GameObject			m_serializePrefab;

		private				byte[]				m_BSSerializationData				= null;

		[SerializeField] 
		private 			int					m_executionCount					= 100000;
		private 			string				m_testResult						= string.Empty;

		#endregion

		#region Constants

		private 	const 	string				kBinarySerializationID				= "binary-serialization";			
		private 	const 	string				kRuntimeSerializationClassID		= "rs-class";			
		private 	const 	string				kGameobjectSaveID					= "rs-gameobject";		
		private 	const 	string				kPrefabSaveID						= "rs-prefab";			

		#endregion

		#region Unity Methods

		private void Start ()
		{
			// Initialise
			m_benchmarkSample			= new BenchmarkSample(true);

#if !(UNITY_WEBPLAYER || UNITY_WEBGL)
			Environment.SetEnvironmentVariable("MONO_REFLECTION_SERIALIZER", "yes");
#endif
			// Load BS data
			LoadBSData();
		}

		private void OnDestroy ()
		{
			SaveBSData();
		}

		private void OnGUI ()
		{
			GUILayoutOption _minWidth	= GUILayout.MinWidth(Screen.width);
			GUILayoutOption _minHeight	= GUILayout.MinHeight(Screen.height * 0.08f);

			GUILayout.BeginArea(new Rect(0f, 0f, Screen.width, Screen.height));
			{
				if (GUILayout.Button("BS Serialization", _minWidth, _minHeight))
				{
					StartProfiling(TestCase.BS_SERIALIZATION);
				}

				if (GUILayout.Button("BS DeSerialization", _minWidth, _minHeight))
				{
					StartProfiling(TestCase.BS_DESERIALIZATION);
				}

				if (GUILayout.Button("RS Serialization (Custom Class)", _minWidth, _minHeight))
				{
					StartProfiling(TestCase.RS_SERIALIZATION_CLASS);
				}

				if (GUILayout.Button("RS DeSerialization (Custom Class)", _minWidth, _minHeight))
				{
					StartProfiling(TestCase.RS_DESERIALIZATION_CLASS);
				}

				if (GUILayout.Button("RS Serialization (GameObject)", _minWidth, _minHeight))
				{
					StartProfiling(TestCase.RS_SERIALIZATION_GAMEOBJECT);
				}
				
				if (GUILayout.Button("RS DeSerialization (GameObject)", _minWidth, _minHeight))
				{
					StartProfiling(TestCase.RS_DESERIALIZATION_GAMEOBJECT);
				}

				if (GUILayout.Button("RS Serialization (Prefab)", _minWidth, _minHeight))
				{
					StartProfiling(TestCase.RS_SERIALIZATION_PREFAB);
				}
				
				if (GUILayout.Button("RS DeSerialization (Prefab)", _minWidth, _minHeight))
				{
					StartProfiling(TestCase.RS_DESERIALIZATION_PREFAB);
				}

				GUILayout.FlexibleSpace();
				GUILayout.BeginHorizontal();
				{
					GUILayout.FlexibleSpace();
					GUILayout.Label(m_testResult);
					GUILayout.FlexibleSpace();
				}
				GUILayout.EndHorizontal();
			}
			GUILayout.EndArea();
		}

		#endregion

		#region Profiling Methods

		private void StartProfiling (TestCase _test)
		{
			Stopwatch _sw = new Stopwatch();
			
			// Start timer to check performance
			_sw.Start();

			switch (_test)
			{
			case TestCase.BS_SERIALIZATION:
				for (int _iter = 0; _iter < m_executionCount; _iter++)
					PerformBSSerialization<BenchmarkSample>(m_benchmarkSample);
				break;

			case TestCase.BS_DESERIALIZATION:
				for (int _iter = 0; _iter < m_executionCount; _iter++)
					m_benchmarkSample		= PerformBSDeserialization<BenchmarkSample>();
				break;

			case TestCase.RS_SERIALIZATION_CLASS:
				for (int _iter = 0; _iter < m_executionCount; _iter++)
					RSManager.Serialize<BenchmarkSample>(m_benchmarkSample, kRuntimeSerializationClassID, eSaveTarget.PLAYER_PREFS);
				break;

			case TestCase.RS_DESERIALIZATION_CLASS:
				for (int _iter = 0; _iter < m_executionCount; _iter++)
					m_benchmarkSample		= RSManager.Deserialize<BenchmarkSample>(kRuntimeSerializationClassID);
				break;

			case TestCase.RS_SERIALIZATION_GAMEOBJECT:
#if (UNITY_WEBPLAYER || UNITY_WEBGL)
				for (int _iter = 0; _iter < m_executionCount; _iter++)
					RSManager.Serialize<GameObject>(m_serializeGameObject, kGameobjectSaveID, eSaveTarget.PLAYER_PREFS);
#else
				for (int _iter = 0; _iter < m_executionCount; _iter++)
					RSManager.Serialize<GameObject>(m_serializeGameObject, kGameobjectSaveID, eSaveTarget.FILE_SYSTEM);
#endif
				break;
				
			case TestCase.RS_DESERIALIZATION_GAMEOBJECT:
				for (int _iter = 0; _iter < m_executionCount; _iter++)
					m_serializeGameObject	= RSManager.Deserialize<GameObject>(kGameobjectSaveID);
				break;

			case TestCase.RS_SERIALIZATION_PREFAB:
#if (UNITY_WEBPLAYER || UNITY_WEBGL)
				for (int _iter = 0; _iter < m_executionCount; _iter++)
					RSManager.Serialize<GameObject>(m_serializePrefab, kPrefabSaveID, eSaveTarget.PLAYER_PREFS);
#else
				for (int _iter = 0; _iter < m_executionCount; _iter++)
					RSManager.Serialize<GameObject>(m_serializePrefab, kPrefabSaveID, eSaveTarget.FILE_SYSTEM);
#endif
				break;
				
			case TestCase.RS_DESERIALIZATION_PREFAB:
				for (int _iter = 0; _iter < m_executionCount; _iter++)
					m_serializePrefab		= RSManager.Deserialize<GameObject>(kPrefabSaveID);
				break;
			}	

			// Stop timer 
			_sw.Stop();
			
			m_testResult	= string.Format("[PerformanceTester] Execution time for {0}: {1}ms", _test, _sw.ElapsedMilliseconds);
		}

		private void PerformBSSerialization <T> (T _object)
		{
			if (_object == null)
			{
				UnityEngine.Debug.LogWarning("[Profiler] BS Serialization failed");
				return;
			}

			using (MemoryStream _memoryStream	= new MemoryStream())
			{
				BinaryFormatter _bf 			= new BinaryFormatter();

				// Serialize
				_bf.Serialize(_memoryStream, _object);
				
				// Cache byte data
				m_BSSerializationData			= _memoryStream.ToArray();
			}
		}

		private T PerformBSDeserialization <T> ()
		{
			if (m_BSSerializationData == null)
			{
				UnityEngine.Debug.LogWarning("Serialization data not found");
				return default(T);
			}

			using (MemoryStream _memoryStream	= new MemoryStream(m_BSSerializationData))
			{
				BinaryFormatter _bf 			= new BinaryFormatter();

				// Deserialize
				return (T)_bf.Deserialize(_memoryStream);
			}
		}

		private void LoadBSData ()
		{
			string _serializationDataBase64		= PlayerPrefs.GetString(kBinarySerializationID, null);
			byte[] _serializationData			= null;

			if (!string.IsNullOrEmpty(_serializationDataBase64))
				_serializationData				= Convert.FromBase64String(_serializationDataBase64);

			m_BSSerializationData				= _serializationData;
		}
		
		private void SaveBSData ()
		{	
			string _serializationDataBase64		= null;

			if (m_BSSerializationData != null)
				_serializationDataBase64		= Convert.ToBase64String(m_BSSerializationData);

			PlayerPrefs.SetString(kBinarySerializationID, _serializationDataBase64);
		}

		#endregion
	}
}