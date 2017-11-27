using UnityEngine;
using System.Collections;
using System;
using System.IO;
using System.Collections.Generic;
using VoxelBusters.Utility;
using VoxelBusters.DesignPatterns;
using VoxelBusters.RuntimeSerialization;
using VoxelBusters.RuntimeSerialization.Internal;

#if UNITY_EDITOR
using UnityEditor;
#endif

/// <summary>
/// RSManager class is responsible for serializing and deserializing objects at runtime.
/// </summary>
#if UNITY_EDITOR
[InitializeOnLoad]
#endif
public class RSManager : SingletonPattern <RSManager>
{
	#region Constants
	
	private 	const		string							kKeyForSaveToPrefsKeyCollection		= "rs-pref-ids";
	private 	const		string							kKeyForSaveToFileKeyCollection		= "rs-files-ids";

	#endregion

	#region Fields

	private					Dictionary<string, List<IRuntimeSerializationEventListener>>	m_eventListeners;
	private					Dictionary<string, byte[]>		m_saveToPrefsSerializationData;
	private					List<string>					m_saveToFileKeyCollection;
	
	// Sub systems
	private 				BinarySerializer				m_binarySerializer;
	private 				BinaryDeserializer				m_binaryDeserializer;

	#endregion
	
	#region Events
	
	public		static		Action							OnDestroyEvent					= null;
	
	#endregion

	#region Constructors

#if UNITY_EDITOR
	static RSManager ()
	{
		EditorApplication.playmodeStateChanged 	-= ApplicationStateChanged;
		EditorApplication.playmodeStateChanged	+= ApplicationStateChanged;
		EditorApplication.update				-= ApplicationUpdate;
		EditorApplication.update				+= ApplicationUpdate;
	}
#endif

	#endregion

	#region Unity Methods

	protected override void Init ()
	{
		base.Init();

		if (instance != this)
			return;

		// Initialise
		m_saveToPrefsSerializationData	= new Dictionary<string, byte[]>();
		m_saveToFileKeyCollection		= new List<string>();
		m_eventListeners				= new Dictionary<string, List<IRuntimeSerializationEventListener>>();
		m_binarySerializer				= new BinarySerializer();
		m_binaryDeserializer			= new BinaryDeserializer();

		// Initialise utility classes
		UnityObjectSerializationUtil.Initialise();
		SerializationTypeUtil.Initialise();

		// Load all the serialization data
		LoadSerializationData();
	}
	
	private void OnApplicationPause (bool _isPaused)
	{
#if !UNITY_EDITOR
		// Save serialization data when application is moved to paused state
		if (_isPaused)
			Obj_Save();
#endif
	}

	protected override void Reset ()
	{
		base.Reset();

		// Reset properties
		m_saveToPrefsSerializationData	= null;
		m_saveToFileKeyCollection		= null;
		m_eventListeners				= null;
		m_binarySerializer				= null;
		m_binaryDeserializer			= null;
	}
	
	protected override void OnDestroy ()
	{
		if (instance == this)
		{
			// Trigger callback
			if (OnDestroyEvent != null)
				OnDestroyEvent();
			
			// Save serialization data
			Obj_Save();
		}
	
		base.OnDestroy();
	}

    #endregion

    #region Serialize [No Save] Methods

    /// <summary>
    /// Returns serialization data of target object as Base64 string. 
    /// After serialization, associated serialization data doesn't get saved by <see cref="RSManager">. And it is user's responsiblity to provide this data while deserializing object using method <see cref="DeserializeData"/> .
    /// </summary>
    /// <param name="_object">The object to be serialized.</param>
    /// <param name="_key">A key string used to identify object's serialization. An optional parameter, when supplied it is used for firing serialization finished callback.</param>
    /// <typeparam name="T">The type of the object being serialized.</typeparam>

    public static string SerializeForMultiThreading<T>(T _object, string _key = null)
    {
        string retVal = "working...";
#if UNITY_EDITOR
        try
        {
#endif
            retVal = Serialize<T>(_object, _key);
#if UNITY_EDITOR
        }
        catch (Exception exception)
        {
            retVal = "ERROR";
            Debug.Log(exception);
        }
#endif
        return retVal;
    }

    public static string Serialize<T> (T _object, string _key = null)
	{
		RSManager	_sharedInstance		= Instance;

		if (_sharedInstance == null)
		{
			Debug.LogError("[RS] The operation could not be completed because RSManager instance is null.");
			return null;
		}

		return _sharedInstance.Obj_Serialize<T>(_object, _key);
	}
	
	private string Obj_Serialize<T> (T _object, string _key)
	{
		// Serialize object 
		byte[] 		_serializationData	= m_binarySerializer.Serialize(_object, typeof(T));
        // Invoke serialization finished object
        if (!string.IsNullOrEmpty(_key))
			OnSerializationFinished(_key, _object);
		
		return Convert.ToBase64String(_serializationData);
	}

	/// <summary>
	/// Deserializes the serialization data and recreates the object of specified type.
	/// </summary>
	/// <returns>The deserialized object of specified type.</returns>
	/// <param name="_serializationDataString">Serialization data in Base64String format.</param>
	/// <param name="_key">A key string used to identify object's deserialization. An optional parameter, when supplied it is used for firing deserialization finished callback.</param>
	/// <param name="_targetObject">If value is non-null, then all the properties are deserialized back to this object.</param>
	/// <typeparam name="T">The type of the object returned after it is deserialized.</typeparam>
	public static T DeserializeData<T> (string _serializationDataString, string _key = null, T _targetObject = default(T))
	{
		RSManager	_sharedInstance		= Instance;
		
		if (_sharedInstance == null)
		{
			Debug.LogError("[RS] The operation could not be completed because RSManager instance is null.");
			return default(T);
		}

		
		return _sharedInstance.Obj_DeserializeData<T>(_serializationDataString, _key, _targetObject);
	}
	
	public T Obj_DeserializeData<T> (string _serializationDataString, string _key, T _targetObject)
	{
		if (string.IsNullOrEmpty(_serializationDataString))
		{
			Debug.LogError("[RS] The operation could not be completed because serialization data is invalid.");
			return default(T);
		}
		
		// Deserialise serialization data
		byte[] 		_serializationData	= Convert.FromBase64String(_serializationDataString);
		T 			_deserializedObject	= m_binaryDeserializer.Deserialize<T>(_serializationData, _targetObject);
		
		// Invoke finished event
		if (!string.IsNullOrEmpty(_key))
			OnDeserializationFinished(_key, _deserializedObject);
		
		return _deserializedObject;
	}

	#endregion

	#region Serialize [With Save] Methods

	/// <summary>
	/// Serialize the specified object and then save its serialization data to mentioned target location. Also given key value is used for identifing objects serialization data. 
	/// </summary>
	/// <param name="_object">The object to be serialized.</param>
	/// <param name="_key">A key string used to identify object's serialization.</param>
	/// <param name="_saveTarget">Preferred target where serialization data is saved.</param>
	/// <typeparam name="T">The type of the object being serialized.</typeparam>
	public static void Serialize<T> (T _object, string _key, eSaveTarget _saveTarget)
	{
		RSManager	_sharedInstance		= Instance;
		
		if (_sharedInstance == null)
		{
			Debug.LogError("[RS] The operation could not be completed because RSManager instance is null.");
			return;
		}

		_sharedInstance.Obj_Serialize<T>(_object, _key, _saveTarget);
	}
	
	private void Obj_Serialize<T> (T _object, string _key, eSaveTarget _saveTarget)
	{
		// Check if key is valid
		if (string.IsNullOrEmpty(_key))
		{
			Debug.LogError("[RS] The operation could not be completed because key is invalid.");
			return;
		}
		
		// Serialize object
		byte[] 		_serializationData	= m_binarySerializer.Serialize(_object, typeof(T));
		
		// Cache serialization data and invoke finished event
		AddSerializationData(_key, _serializationData, _saveTarget);
		OnSerializationFinished(_key, _object);
	}

	/// <summary>
	/// Deserializes the data serialization associated with given key and recreates the object of specified type.
	/// </summary>
	/// <param name="_key">A key string used to identify object's deserialization.</param>
	/// <param name="_targetObject">If value is non-null, then all the properties are deserialized back to this object.</param>
	/// <typeparam name="T">The type of the object returned after it is deserialized.</typeparam>
	public static T Deserialize<T> (string _key, T _targetObject = default(T))
	{
		RSManager	_sharedInstance		= Instance;
		
		if (_sharedInstance == null)
		{
			Debug.LogError("[RS] The operation could not be completed because RSManager instance is null.");
			return default(T);
		}

		return _sharedInstance.Obj_Deserialize<T>(_key, _targetObject);
	}

	private T Obj_Deserialize<T> (string _key, T _targetObject)
	{
		byte[] 		_serializationData	= null;
		
		// Get serialization data
		if (!Obj_TryGetSerializationData(_key, out _serializationData))
		{
			Debug.LogError(string.Format("[RS] The operation could not be completed because serialization data with Key: {0} not found.", _key));
			return default(T);
		}

		// Deserialize data
		T 			_deserializedObject	= m_binaryDeserializer.Deserialize<T>(_serializationData, _targetObject);
		
		// Invoke finished event
		OnDeserializationFinished(_key, _deserializedObject);
		
		return _deserializedObject;
	}

	#endregion

	#region Serialization Data Methods

	/// <summary>
	/// Determines whether <see cref="RSManager"/> contains serialization data for specified key.
	/// </summary>
	/// <returns><c>true</c>, if serialization data was found, <c>false</c> otherwise.</returns>
	/// <param name="_key">A key string used to identify object's serialization data.</param>
	public static bool ContainsKey (string _key)
	{
		RSManager	_sharedInstance		= Instance;
		
		if (_sharedInstance == null)
		{
			Debug.LogError("[RS] The operation could not be completed because RSManager instance is null.");
			return false;
		}
		
		return _sharedInstance.Obj_ContainsKey(_key);
	}
	
	private bool Obj_ContainsKey (string _key)
	{
		if (m_saveToPrefsSerializationData.ContainsKey(_key))
			return true;
		
		return m_saveToFileKeyCollection.Contains(_key);
	}

	/// <summary>
	/// Returns serialization data associated with specified key.
	/// Ideal for supporting multi device login, wherein you can save serialization data remotely and restore it on other devices using method <see cref="RSManager.RestoreSerializationData"/>.
	/// </summary>
	/// <returns>Serialization data as Base64String.</returns>
	/// <param name="_key">A key string used to identify object's serialization data.</param>
	public static string GetSerializationData (string _key)
	{
		RSManager	_sharedInstance		= Instance;
		
		if (_sharedInstance == null)
		{
			Debug.LogError("[RS] The operation could not be completed because RSManager instance is null.");
			return null;
		}

		return _sharedInstance.Obj_GetSerializationData(_key);
	}

	private string Obj_GetSerializationData (string _key)
	{
		byte[] 		_serializationData	= null;

		if (Obj_TryGetSerializationData(_key, out _serializationData))
			return Convert.ToBase64String(_serializationData);

		throw new Exception(string.Format("[RS] The operation could not be completed because serialization data with Key: {0} not found.", _key));
	}

	public static bool TryGetSerializationData (string _key, out byte[] _serializationData)
	{
		RSManager	_sharedInstance		= Instance;
		
		if (_sharedInstance == null)
		{
			Debug.LogError("[RS] The operation could not be completed because RSManager instance is null.");
			_serializationData	= null;

			return false;
		}
		
		return _sharedInstance.Obj_TryGetSerializationData(_key, out _serializationData);
	}

	private bool Obj_TryGetSerializationData (string _key, out byte[] _serializationData)
	{
		// Try to get serialization data from player prefs
		if (m_saveToPrefsSerializationData.TryGetValue(_key, out _serializationData))
			return true;
		
		// Try to get serialization data from files
		string		_filePath		= GetSerializationDataSavePathForKey(_key);
		bool		_fileExists		= FileOperations.Exists(_filePath);
		
		if (_fileExists)
			_serializationData		= FileOperations.ReadAllBytes(_filePath);
		
		return _fileExists;
	}

	/// <summary>
	/// Saves Base64String format serialization data to specified target location where it is associated with specified key.
	/// </summary>
	/// <param name="_serializationDataString">Serialization data in Base64String format.</param>
	/// <param name="_key">A key string used to identify object's serialization data.</param>
	/// <param name="_saveTarget">Serialization data save target.</param>
	public static bool RestoreSerializationData (string _serializationDataString, string _key, eSaveTarget _saveTarget)
	{
		RSManager	_sharedInstance		= Instance;
		
		if (_sharedInstance == null)
		{
			Debug.LogError("[RS] The operation could not be completed because RSManager instance is null.");
			return false;
		}

		return _sharedInstance.Obj_RestoreSerializationData(_serializationDataString, _key, _saveTarget);
	}

	private bool Obj_RestoreSerializationData (string _serializationDataString, string _key, eSaveTarget _saveTarget)
	{
		if (string.IsNullOrEmpty(_serializationDataString))
		{
			Debug.LogError("[RS] The operation could not be completed because serialization data is invalid.");
			return false;
		}

		// Check if serialization data is compatible with code
		byte[]		_serializationData			= Convert.FromBase64String(_serializationDataString);
		int 		_serializedFormatVersion	= m_binaryDeserializer.GetVersion(_serializationData);

		if (_serializedFormatVersion <= Constants.kSerializationSupportedFormatVersions)
		{
			AddSerializationData(_key, _serializationData, _saveTarget);
			return true;
		}

		Debug.LogWarning("[RS] The operation could not be completed because serialization data format not supported. Please update SDK to the most recent version.");
		return false;
	}

	private void AddSerializationData (string _key, byte[] _serializationData, eSaveTarget _saveTarget)
	{
		if (_saveTarget == eSaveTarget.PLAYER_PREFS)
		{
			m_saveToPrefsSerializationData[_key]	= _serializationData;
		}
		else
		{
			string		_serializationDataSavePath	= GetSerializationDataSavePathForKey(_key);
			
			// Cache key information
			if (!m_saveToFileKeyCollection.Contains(_key))
				m_saveToFileKeyCollection.Add(_key);
			
			// Save serialization data to file				
			FileOperations.WriteAllBytes(_serializationDataSavePath, _serializationData);
		}
	}

	#endregion

	#region Event Listener Methods

	/// <summary>
	/// Register object to receive serialization events.
	/// </summary>
	/// <param name="_key">A key string used to identify object serialization and deserialization.</param>
	/// <param name="_newListener">Register listener object for the events associated with given key.</param>
	public static void RegisterEventListener (string _key, IRuntimeSerializationEventListener _newListener)
	{
		RSManager	_sharedInstance		= Instance;
		
		if (_sharedInstance == null)
		{
			Debug.LogError("[RS] The operation could not be completed because RSManager instance is null.");
			return;
		}

		_sharedInstance.Obj_RegisterEventListener(_key, _newListener);
	}

	private void Obj_RegisterEventListener (string _key, IRuntimeSerializationEventListener _newListener)
	{
		if (string.IsNullOrEmpty(_key))
		{
			Debug.LogError("[RS] The operation could not be completed because key value is invalid.");
			return;
		}

		if (_newListener == null)
		{
			Debug.LogError("[RS] The operation could not be completed because listener object is null.");
			return;
		}

		// Get the listener list associated with this key
		List<IRuntimeSerializationEventListener> _eventListeners	= GetEventListeners(_key);

		if (_eventListeners == null)
		{
			_eventListeners			= new List<IRuntimeSerializationEventListener>();
			m_eventListeners[_key]	= _eventListeners;
		}

		// Registering observer
		if (!_eventListeners.Contains(_newListener))
			_eventListeners.Add(_newListener);
	}

	/// <summary>
	/// Unregister object from receiving serialization events.
	/// </summary>
	/// <param name="_key">A key string used to identify object serialization and deserialization.</param>
	/// <param name="_newListener">Unregister listener object from the events associated with given key.</param>
	public static void UnRegisterEventListener (string _key, IRuntimeSerializationEventListener _listener)
	{
		RSManager	_sharedInstance		= Instance;
		
		if (_sharedInstance == null)
		{
			Debug.LogError("[RS] The operation could not be completed because RSManager instance is null.");
			return;
		}

		_sharedInstance.Obj_UnRegisterEventListener(_key, _listener);
	}

	private void Obj_UnRegisterEventListener (string _key, IRuntimeSerializationEventListener _listener)
	{
		if (string.IsNullOrEmpty(_key))
		{
			Debug.LogError("[RS] The operation could not be completed because key value is invalid.");
			return;
		}

		if (_listener == null)
		{
			Debug.LogError("[RS] The operation could not be completed because listener object is null.");
			return;
		}

		// Get the listener list associated with this key
		List<IRuntimeSerializationEventListener> _eventListeners	= GetEventListeners(_key);

		// Remove observer
		if (_eventListeners != null)
			_eventListeners.Remove(_listener);
	}

	private List<IRuntimeSerializationEventListener> GetEventListeners (string _key)
	{
		List<IRuntimeSerializationEventListener> _eventListeners;
		
		// Fetch callback list associated with this key
		m_eventListeners.TryGetValue(_key, out _eventListeners);
		
		return _eventListeners;
	}
	
	private void OnSerializationFinished (string _key, object _object)
	{
		// Get the listener list associated with this key
		List<IRuntimeSerializationEventListener> 	_eventListeners	= GetEventListeners(_key);
		
		if (_eventListeners == null)
			return;
		
		foreach (IRuntimeSerializationEventListener _listener in _eventListeners)
		{
			if (_listener != null)
				_listener.OnAfterRuntimeSerialize(_key, _object);
		}
	}
	
	private void OnDeserializationFinished (string _key, object _object)
	{
		// Get the listener list associated with this key
		List<IRuntimeSerializationEventListener> 	_eventListeners	= GetEventListeners(_key);
		
		if (_eventListeners == null)
			return;
		
		foreach (IRuntimeSerializationEventListener _listener in _eventListeners)
		{
			if (_listener != null)
				_listener.OnAfterRuntimeDeserialize(_key, _object);
		}
	}

	#endregion

	#region Load Methods

	private void LoadSerializationData ()
	{
		LoadDataRelatedToSaveTargetPrefs();

#if !(UNITY_WEBPLAYER || UNITY_WEBGL)
		LoadDataRelatedToSaveTargetFile();
#endif
	}
	
	private void LoadDataRelatedToSaveTargetPrefs ()
	{
		List<string>	_keyCollection	= LoadKeyCollectionInfo(kKeyForSaveToPrefsKeyCollection);
		
		if (_keyCollection == null)
			return;
		
		foreach (string _key in _keyCollection)
		{
			string		_serializationDataString	= PlayerPrefs.GetString(_key, null);

			if (string.IsNullOrEmpty(_serializationDataString))
				continue;

			// Add it to dictionary
			m_saveToPrefsSerializationData.Add(_key, Convert.FromBase64String(_serializationDataString));
		}
	}

#if !(UNITY_WEBPLAYER || UNITY_WEBGL)
	private void LoadDataRelatedToSaveTargetFile ()
	{
		List<string> 	_keyCollection	= LoadKeyCollectionInfo(kKeyForSaveToFileKeyCollection);
		
		// Update value
		if (_keyCollection != null)
			m_saveToFileKeyCollection	= _keyCollection;
	}
#endif

	private List<string> LoadKeyCollectionInfo (string _key)
	{
		string 			_keyCollectionString	= PlayerPrefs.GetString(_key);
		
		if (string.IsNullOrEmpty(_keyCollectionString))
			return null;
		
		// Read all saved key collection info
		using (MemoryStream _memoryStream = new MemoryStream(Convert.FromBase64String(_keyCollectionString)))
		{
			using (RSBinaryReader _keyCollectionReader = new RSBinaryReader(_memoryStream))
			{
				int 			_keysCount		= _keyCollectionReader.ReadInt32();
				List<string> 	_keyCollection	= new List<string>(_keysCount);
				
				for (int _iter = 0; _iter < _keysCount; _iter++)
					_keyCollection.Add(_keyCollectionReader.ReadString());

				return _keyCollection;
			}
		}
	}

	#endregion

	#region Save Methods

	/// <summary>
	/// Writes all serialization data to disk.
	/// By default <see cref="RSManager"/> writes serialization data to PlayerPrefs/File on Application Pause and on Application Quit.
	/// </summary>
	public static void Save ()
	{
		RSManager	_sharedInstance		= Instance;
		
		if (_sharedInstance == null)
		{
			Debug.LogError("[RS] The operation could not be completed because RSManager instance is null.");
			return;
		}
		
		_sharedInstance.Obj_Save();
	}

	private void Obj_Save ()
	{
		SaveDataRelatedToSaveTargetPrefs();

#if !(UNITY_WEBPLAYER || UNITY_WEBGL)
		SaveDataRelatedToSaveTargetFile();
#endif

		// Save player prefs
		PlayerPrefs.Save();
	}

	private void SaveDataRelatedToSaveTargetPrefs ()
	{
		using (MemoryStream _memoryStream = new MemoryStream(256))
		{
			using (RSBinaryWriter _keyCollectionWriter = new RSBinaryWriter(_memoryStream))
			{
				Dictionary<string, byte[]>.Enumerator	_dictEnumerator		= m_saveToPrefsSerializationData.GetEnumerator();

				// Write key count
				_keyCollectionWriter.Write(m_saveToPrefsSerializationData.Count);
				
				// Write key info and add serialization data to the player prefs
				while (_dictEnumerator.MoveNext())
				{
					KeyValuePair<string, byte[]> 	_keyValuePair		= _dictEnumerator.Current;
					string 							_serializationKey	= _keyValuePair.Key;
					byte[]							_serializationData	= _keyValuePair.Value;
					
					if (_serializationData == null)
						continue;

					// Save serialization data to the player prefs using its associated key
					PlayerPrefs.SetString(_serializationKey, Convert.ToBase64String(_serializationData));
					
					// Add cuurent key to key collection
					_keyCollectionWriter.Write(_serializationKey);
				}

				// Save key collection info to player prefs
				PlayerPrefs.SetString(kKeyForSaveToPrefsKeyCollection, _keyCollectionWriter.ToBase64String());
			}
		}
	}

#if !(UNITY_WEBPLAYER || UNITY_WEBGL)
	private void SaveDataRelatedToSaveTargetFile ()
	{
		using (MemoryStream _memoryStream = new MemoryStream(256))
		{
			using (RSBinaryWriter _keyCollectionWriter = new RSBinaryWriter(_memoryStream))
			{
				int 	_keysCount	= m_saveToFileKeyCollection.Count;

				// Write keys count
				_keyCollectionWriter.Write(_keysCount);
				
				// Write all the keys
				for (int _iter = 0; _iter < _keysCount; _iter++)
					_keyCollectionWriter.Write(m_saveToFileKeyCollection[_iter]);
				
				// Save key collection info to player prefs
				PlayerPrefs.SetString(kKeyForSaveToFileKeyCollection, _keyCollectionWriter.ToBase64String());
			}
		}
	}
#endif

	#endregion

	#region Remove Methods
	
	/// <summary>
	/// Removes serialization data associated with specified key.
	/// </summary>
	/// <param name="_key">A key string used to identify object's serialization data.</param>
	public static void Remove (string _key)
	{
		RSManager	_sharedInstance		= Instance;
		
		if (_sharedInstance == null)
		{
			Debug.LogError("[RS] The operation could not be completed because RSManager instance is null.");
			return;
		}

		_sharedInstance.Obj_Remove(_key);
	}

	private void Obj_Remove (string _key)
	{
		// If given key exists within serialized data saved to prefs then remove it
		if (m_saveToPrefsSerializationData.Remove(_key))
			return;

		// If given key exists in serialized to files list, then remove it
		if (m_saveToFileKeyCollection.Remove(_key))
			RemoveSerializationDataFileWithKey(_key);
	}
	
	/// <summary>
	/// Clears all existing serialization data.
	/// </summary>
	public static void RemoveAll ()
	{
		RSManager	_sharedInstance		= Instance;
		
		if (_sharedInstance == null)
		{
			Debug.LogError("[RS] The operation could not be completed because RSManager instance is null.");
			return;
		}

		_sharedInstance.Obj_RemoveAll();
	}

	private void Obj_RemoveAll ()
	{
		// Remove all serialization data which are saved to prefs
		m_saveToPrefsSerializationData.Clear();

		// Remove all serialization data which were saved as files
		for (int _iter = 0; _iter < m_saveToFileKeyCollection.Count; _iter++)
			RemoveSerializationDataFileWithKey(m_saveToFileKeyCollection[_iter]);

		m_saveToFileKeyCollection.Clear();
	}

	#endregion

	#region Purge Methods

	/// <summary>
	/// Clears all the cached information of specified object type.
	/// </summary>
	public static void Purge (Type _objectType)
	{
		SerializationTypeUtil.Purge(_objectType);
	}

	#endregion

	#region Misc. Methods

	/// <summary>
	/// Gets the serialization format version.
	/// </summary>
	/// <returns>The serialization format version.</returns>
	public int GetSerializationFormatVersion ()
	{
		return Constants.kSerializationFormatVersion;
	}

	private string GetSerializationDataSavePathForKey (string _key)
	{
		string	_fileName	= "RS" + _key + ".txt";
		string 	_filePath	= Path.Combine(Application.persistentDataPath, _fileName);

		return _filePath;
	}

	private bool RemoveSerializationDataFileWithKey (string _key)
	{
		string	_filePath	= GetSerializationDataSavePathForKey(_key);
		bool	_fileExists	= FileOperations.Exists(_filePath);

		if (_fileExists)
			FileOperations.Delete(_filePath);

		return _fileExists;
	}

	#endregion

	#region Editor Methods

#if UNITY_EDITOR
	private static void ApplicationUpdate ()
	{
		EditorApplication.update	-= ApplicationUpdate;

		// Assembly reload might have occured
		ApplicationStateChanged();
	}

	private static void ApplicationStateChanged ()
	{
		if (EditorApplicationIsPlaying())
			return;

		// Reset properties to ensure RSManager works fine in Edit mode as well
		instance					= null;
		destroyedOnApplicationQuit	= false;

		// Reset instance properties
		RSManager	_sceneInstance	= FindObjectOfType<RSManager>();

		if (_sceneInstance != null)
			_sceneInstance.Reset();
	}

	private static bool EditorApplicationIsPlaying ()
	{
		return (EditorApplication.isPlaying || EditorApplication.isPlayingOrWillChangePlaymode || EditorApplication.isPaused);
	}
#endif

	#endregion
}