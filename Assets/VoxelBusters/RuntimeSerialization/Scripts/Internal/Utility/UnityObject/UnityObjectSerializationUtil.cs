using UnityEngine;
using System.Collections;
using System;
using System.Collections.Generic;
using System.Text;
using TMPro;
using VoxelBusters.Utility;

namespace VoxelBusters.RuntimeSerialization.Internal
{
	public partial class UnityObjectSerializationUtil 
	{
		#region Properties

		private		static		Dictionary<GameObject, UIDSystem>			uidSystemCache;
		private		static		Dictionary<GameObject, string>				hierarchyMetadataCache;

		// Ghost prefab
		private		static		UIDSystem									ghostPrefabContainerUIDSystem;
		private		static		UIDSystem									rsManagerUIDSystem;

		#endregion

		#region Constants

		private		const		string										kGhostPrefabContainerName	= "RSGhostPrefabs";
		private		const		string										kGhostPrefabContainerUID	= "RS-GhostPrefabs";

		#endregion

		#region Constructors

		static UnityObjectSerializationUtil ()
		{
			uidSystemCache			= new Dictionary<GameObject, UIDSystem>();
			hierarchyMetadataCache	= new Dictionary<GameObject, string>();
		}

		#endregion

		#region Methods

		internal static void Initialise ()
		{
			// Create ghost prefab parent
			CreateGhostPrefabContainer();

			// Unity generic objects
			RSExtensionManager.AddNewExtension(typeof(GameObject), 		new GameObjectRSExtension());
			RSExtensionManager.AddNewExtension(typeof(MonoBehaviour), 	new MonoBehaviourRSExtension());
			RSExtensionManager.AddNewExtension(typeof(Transform), 		new TransformRSExtension());
			RSExtensionManager.AddNewExtension(typeof(Material), 		new MaterialRSExtension());
			RSExtensionManager.AddNewExtension(typeof(Mesh), 			new MeshRSExtension());
			RSExtensionManager.AddNewExtension(typeof(MeshFilter), 		new MeshFilterRSExtension());
			RSExtensionManager.AddNewExtension(typeof(MeshRenderer), 	new MeshRendererRSExtension());
			RSExtensionManager.AddNewExtension(typeof(Shader), 			new ShaderRSExtension());
			RSExtensionManager.AddNewExtension(typeof(Texture2D), 		new Texture2DRSExtension());
			RSExtensionManager.AddNewExtension(typeof(Bounds), 			new BoundsRSExtension());

			// Unity 3D objects
			RSExtensionManager.AddNewExtension(typeof(Rigidbody), 		new RigidbodyRSExtension());

			// Unity 2D objects
			RSExtensionManager.AddNewExtension(typeof(Rigidbody2D), 	new Rigidbody2DRSExtension());

			// Unity struct objects
			RSExtensionManager.AddNewExtension(typeof(Vector2), 		new Vector2RSExtension());
			RSExtensionManager.AddNewExtension(typeof(Vector3), 		new Vector3RSExtension());
			RSExtensionManager.AddNewExtension(typeof(Vector4), 		new Vector4RSExtension());
			RSExtensionManager.AddNewExtension(typeof(Color), 			new ColorRSExtension());
			RSExtensionManager.AddNewExtension(typeof(Color32), 		new Color32RSExtension());
			RSExtensionManager.AddNewExtension(typeof(Quaternion), 		new QuaternionRSExtension());

            //CalcFlow Types
            RSExtensionManager.AddNewExtension(typeof(TMP_Text),        new TMProExtension());
            RSExtensionManager.AddNewExtension(typeof(TextMeshPro),     new TMProExtension());

			RSExtensionManager.AddNewExtension(typeof(LineRenderer),    new LineRendererExtension());

        }

        internal static void Reset ()
		{
			uidSystemCache.Clear();
			hierarchyMetadataCache.Clear();
		}

		private static void CreateGhostPrefabContainer ()
		{
			UIDSystem		_RSManagerUIDSystem	= RSManager.Instance.GetComponent<UIDSystem>();
			Transform		_RSManagerTransform	= _RSManagerUIDSystem.CachedTransform;
			Transform		_containerTransform	= _RSManagerTransform.Find(kGhostPrefabContainerName);
			GameObject		_containerGO;
			UIDSystem		_containerUIDSystem;

			if (_containerTransform == null)
			{
				_containerGO			= new GameObject(kGhostPrefabContainerName);
				_containerUIDSystem		= _containerGO.AddComponent<UIDSystem>();
			}
			else
			{
				_containerGO			= _containerTransform.gameObject;
				_containerUIDSystem		= _containerGO.GetComponent<UIDSystem>();
			}

			// Set properties
#if RS_DEBUG_MODE
			_containerGO.hideFlags 		= HideFlags.None;
#else
			_containerGO.hideFlags 		= HideFlags.HideInHierarchy;
#endif
			_containerGO.SetActive(false);
			_containerUIDSystem.CachedTransform.SetParent(_RSManagerTransform);
			_containerUIDSystem.SetGameObjectUID(kGhostPrefabContainerUID);

			// Cache reference
			rsManagerUIDSystem				= _RSManagerUIDSystem;
			ghostPrefabContainerUIDSystem	= _containerUIDSystem;
		}

		#endregion

		#region Unity Object Methods

		public static string GetHierarchyMetadata (GameObject _gameObject)
		{
			string 			_hierarchyMetadata			= null;

			// Try to get cached value
			if (hierarchyMetadataCache.TryGetValue(_gameObject, out _hierarchyMetadata))
				return _hierarchyMetadata;

			// Traverse through heirarchy
			Transform 		_gameObjectTransform		= _gameObject.transform;
			StringBuilder 	_hierarchyMetadataBuilder	= new StringBuilder(64);
			
			for (Transform _curTransform = _gameObjectTransform; _curTransform != null;)
			{
				GameObject	_curGameObject				= _curTransform.gameObject;
				UIDSystem	_curObjectUIDSystem			= _curGameObject.GetComponent<UIDSystem>();

				if (_curObjectUIDSystem == null)
					Debug.LogException(new Exception(string.Format("[RS] The operation could not be completed because UIDSystem component is found missing in gameobject with name: {0}.", _curGameObject.name)), _curGameObject);

				string		_curGameObjectUID			= _curObjectUIDSystem.GetGameObjectIdentifier();
				Transform	_parentTransform			= _curTransform.parent;

				// Root gameObject includes extra metadata i.e, its tag
				if (_parentTransform == null)
				{
					_hierarchyMetadataBuilder.Insert(0, _curGameObject.tag);
					_hierarchyMetadataBuilder.Insert(0, ':');
				}

				// Append gameObject's UID and its name
				_hierarchyMetadataBuilder.Insert(0, _curGameObjectUID);
				_hierarchyMetadataBuilder.Insert(0, ':');
				_hierarchyMetadataBuilder.Insert(0, _curGameObject.name);
				_hierarchyMetadataBuilder.Insert(0, '/');

				// Next move to its parent transform
				_curTransform							= _parentTransform;							
			}

			// Cache this information, to avoid repeating same step again for this gameObject
			_hierarchyMetadata		= _hierarchyMetadataBuilder.ToString();
			hierarchyMetadataCache.Add(_gameObject, _hierarchyMetadata);

			return _hierarchyMetadata;
		}
		
		public static GameObject GetGameObject (string _hierarchyMetadata, bool _isPrefab)
		{
			UIDSystem	_uidSystem	= GetUIDSystem(_hierarchyMetadata, _isPrefab);

			return _uidSystem.CachedGameObject;
		}

		public static Component GetComponent (string _hierarchyMetadata, string _componentUID, Type _componentType, bool _isPrefab)
		{
			UIDSystem	_uidSystem	= GetUIDSystem(_hierarchyMetadata, _isPrefab);

			return GetComponent(_uidSystem, _componentUID, _componentType);
		}

		public static Component GetComponent (GameObject _gameObject, string _componentUID, Type _componentType)
		{
			UIDSystem	_uidSystem	= _gameObject.GetComponent<UIDSystem>();

			return GetComponent(_uidSystem, _componentUID, _componentType);
		}

		public static Component GetComponent (UIDSystem _uidSystem, string _componentUID, Type _componentType)
		{
			Component	_component	= _uidSystem.GetComponent(_componentUID);
			
			if (_component != null && _componentType.IsInstanceOfType(_component))
				return _component;
			
			// As component with given UID doesnt exist, please create a new one
			Component	_newComponent	= null;
			
			if (_componentType == typeof(Transform))
				_newComponent		= _uidSystem.CachedTransform;
			else
				_newComponent		= _uidSystem.CachedGameObject.AddComponent(_componentType);
			
			// Update UIDSystem with type and UID information
			_uidSystem.SetComponentUID(_newComponent, _componentUID);
			
			return _newComponent;
		}
		
		#endregion
		
		#region UID Methods

		private static UIDSystem GetUIDSystem (string _hierarchyMetadata, bool _isPrefab)
		{
			string		_trimmedHierarchyMetadata	= _hierarchyMetadata.TrimStart('/');
			string[]	_pathComponents				= _trimmedHierarchyMetadata.Split('/');
			int 		_pathComponentCount			= _pathComponents.Length;

			if (_pathComponentCount == 0)
				throw new Exception(string.Format("[RS] The operation could not be completed because hierarchy metadata is invalid. Processed hierarchy metadata: {0}.", _hierarchyMetadata));

			// First check if cached object has this UID
			string[]	_gameObjectMetatadata	= _pathComponents[_pathComponentCount - 1].Split(':');
			string		_gameObjectUID			= _gameObjectMetatadata[1];
			Dictionary<GameObject, UIDSystem>.Enumerator	_uidSystemEnumerator	= uidSystemCache.GetEnumerator();

			while (_uidSystemEnumerator.MoveNext())
			{
				UIDSystem	_curUIDSystem		= _uidSystemEnumerator.Current.Value;

				if (_curUIDSystem == null)
					continue;

				if (_curUIDSystem.GetGameObjectIdentifier().Equals(_gameObjectUID))
					return _curUIDSystem;
			}

			// To ensure that hierarychy is maintained, lets start from root gameObject and iterate deeper into the target object
			UIDSystem	_rootUIDSystem			= GetRootObjectUIDSystem(_pathComponents[0], _isPrefab);
			UIDSystem	_curObjectUIDSystem		= _rootUIDSystem;

			for (int _iter = 1; _iter < _pathComponentCount; _iter++)
			{
				Transform	_parentTransform	= _curObjectUIDSystem.CachedTransform;
				_curObjectUIDSystem				= GetChildObjectUIDSystem(_parentTransform, _pathComponents[_iter]);
			}

			return _curObjectUIDSystem;
		}

		private static UIDSystem GetRootObjectUIDSystem (string _metadata, bool _isPrefab)
		{
			string[]		_components			= _metadata.Split(':');
			int				_componentsCount	= _components.Length;
			
			if (_componentsCount != 3)
				throw new Exception("[RS] The operation could not be completed because root gameObject name is invalid.");
			
			// Fetch info from metadata
			string			_gameObjectName		= _components[0];
			string			_gameObjectUID		= _components[1];

			// Find root for prefabs
			if (_isPrefab)
			{
				string		_rsManagerUID		= rsManagerUIDSystem.GetGameObjectIdentifier();

				if (_gameObjectUID.Equals(_rsManagerUID))
					return rsManagerUIDSystem;
			
				IEnumerator	_prefabEnumerator	= ghostPrefabContainerUIDSystem.CachedTransform.GetEnumerator();

				while (_prefabEnumerator.MoveNext())
				{
					Transform	_curChildTransform	= (Transform)_prefabEnumerator.Current;
					UIDSystem	_curChildUIDSystem	= _curChildTransform.GetComponent<UIDSystem>();

					if (_gameObjectUID.Equals(_curChildUIDSystem.GetGameObjectIdentifier()))
					{
						return _curChildUIDSystem;
					}
				}

				return OnFindObjectFailed(_gameObjectName, _gameObjectUID, ghostPrefabContainerUIDSystem.CachedTransform);
			}

			// Find root for gameObjects
			string			_gameObjectTag		= _components[2];
			GameObject[]	_gameObjectList		= null;
			
			if (_gameObjectTag.Equals("Untagged"))
				_gameObjectList					= GameObject.FindObjectsOfType<GameObject>();
			else
				_gameObjectList					= GameObject.FindGameObjectsWithTag(_gameObjectTag);

			// Iterate through the list and find our target object
			int				_gameObjectCount	= _gameObjectList.Length;

			for (int _iter = 0; _iter < _gameObjectCount; _iter++)
			{
				GameObject	_curGameObject		= _gameObjectList[_iter];
				UIDSystem	_curObjectUIDSystem	= _curGameObject.GetComponent<UIDSystem>();
				
				// If a gameobject was previously serialized then it should be having UIDSystem
				if (_curObjectUIDSystem == null)
					continue;
				
				// Check if there is UID match
				if (_curObjectUIDSystem.GetGameObjectIdentifier().Equals(_gameObjectUID))
					return OnFindObjectSuccess(_curGameObject, _curObjectUIDSystem, _gameObjectName);
			}
			
			// Root object doesnt exist, take appropriate actions
			return OnFindObjectFailed(_gameObjectName, _gameObjectUID, null);
		}

		private static UIDSystem GetChildObjectUIDSystem (Transform _parentTransform, string _metadata)
		{
			string[]		_components			= _metadata.Split(':');
			int				_componentsCount	= _components.Length;

			if (_componentsCount != 2)
				throw new Exception("[RS] Root gameObject name is invalid.");

			// Iterate through immediate child and find gameobject with given UID
			string			_gameObjectName		= _components[0];
			string			_gameObjectUID		= _components[1];

			foreach (Transform _childTransform in _parentTransform)
			{
				UIDSystem	_curObjectUIDSystem = _childTransform.GetComponent<UIDSystem>();

				if (_curObjectUIDSystem == null)
					continue;

				if (_curObjectUIDSystem.GetGameObjectIdentifier().Equals(_gameObjectUID))
					return OnFindObjectSuccess(_curObjectUIDSystem.CachedGameObject, _curObjectUIDSystem, _gameObjectName);
			}

			return OnFindObjectFailed(_gameObjectName, _gameObjectUID, _parentTransform);
		}

		private static UIDSystem OnFindObjectSuccess (GameObject _gameObject, UIDSystem _uidSystem, string _gameObjectName)
		{
			// Update cache and target object's name
			uidSystemCache[_gameObject]	= _uidSystem;
			_gameObject.name			= _gameObjectName;

			return _uidSystem;
		}

		private static UIDSystem OnFindObjectFailed (string _gameObjectName, string _gameObjectUID, Transform _parentTransform)
		{
			// Create new gameObject as we failed to find object with given UID
			GameObject 	_newGameObject	= new GameObject(_gameObjectName);
			Transform	_newTransform	= _newGameObject.transform;

			// Update parent information
			_newTransform.SetParent(_parentTransform);

			// Attached UIDSystem and assign gameObject's UID
			UIDSystem	_newUIDSystem	= _newGameObject.AddComponent<UIDSystem>();

			_newUIDSystem.SetGameObjectUID(_gameObjectUID);

			return OnFindObjectSuccess(_newGameObject, _newUIDSystem, _gameObjectName);
		}

		#endregion
	}
}