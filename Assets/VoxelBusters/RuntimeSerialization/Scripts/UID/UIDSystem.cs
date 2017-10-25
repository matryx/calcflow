using UnityEngine;
using System.Collections;
using System;
using System.Collections.Generic;
using VoxelBusters.Utility;

namespace VoxelBusters.RuntimeSerialization
{
	/// <summary>
	/// Unique Identifier System is used for uniquely identify a GameObject and Component attached to it.
	/// Please attach this component to all the GameObject that will undergo serialization and deserialization.
	/// When this component is attached to a GameObject, it recursively creates UIDSystem component in all descendants and assigns UID.
	/// All the assigned UID's are cached and used for RS system for identifying an object in Scene hierarchy.
	/// Note that UID for each object, is assigned only once until and unless user forcefully reset's component. 
	/// Please dont forget to enable IsPrefab flag, which is used for differentiating between normal GameObject and Prefab.
	/// UID's for Prefab are generated and assigned at runtime only.
	/// </summary>
	[RuntimeSerializable(typeof(MonoBehaviour), false)]
	public partial class UIDSystem : MonoBehaviour
	{
		#region Constants

		internal	const		string				kMethodUpdateUIDs			= "UpdateUIDs";
		internal	const		string				kMethodReassignUIDs			= "ReassignUIDs";

		private 	const		string				kRSManagerUID				= "RS-Manager";

		#endregion
	
		#region Fields

		[SerializeField]
		private					UnityObjectUIDMap	m_gameObjectUIDMap;
		[SerializeField]
		private					List<UnityObjectUIDMap>	m_componentUIDMapList	= new List<UnityObjectUIDMap>();
		[SerializeField, RuntimeSerializeField]
		private					bool				m_serializeChildren			= true;
		[SerializeField, RuntimeSerializeField]
		[ExecuteOnValueChange(kMethodUpdateUIDs), Tooltip("Mark this enabled only for Prefab.")]
		private					bool				m_isPrefab					= false;

		private					GameObject			m_cGameObject;
		private					Transform			m_cTransform;
		
		#endregion

		#region Properties

		public GameObject CachedGameObject
		{
			get
			{
				if (m_cGameObject == null)
					m_cGameObject	= gameObject;

				return m_cGameObject;
			}
			
			private set
			{
				m_cGameObject	= value;
			}
		}

		public Transform CachedTransform
		{
			get
			{
				if (m_cTransform == null)
					m_cTransform	= transform;
				
				return m_cTransform;
			}

			private set
			{
				m_cTransform	= value;
			}
		}

		public bool IsPrefab
		{
			get
			{
				return m_isPrefab;
			}
			
			internal set
			{
				m_isPrefab	= value;
			}
		}

		public bool SerializeChildren
		{
			get
			{
				return m_serializeChildren;
			}

			set
			{
				m_serializeChildren	= value;
			}
		}

		#endregion

		#region Unity Methods

		private void Awake ()
		{
			// Initialize component
			CachedGameObject	= gameObject;
			CachedTransform		= transform;
			IsPrefab			= false;
		}

		#endregion

		#region Methods
	
		private void Reset ()
		{
			// Reset values
			IsPrefab			= false;
			SerializeChildren	= true;
			
			// Reassign UIDs recursively to all the gameobjects and components attached to it
			ReassignUIDs(true);
		}

		/// <summary>
		/// Assigns new UID for all the new Components attached to this GameObject.
		/// </summary>
		public void UpdateUIDs (bool _recursive)
		{
			// Set new UID for gameobject, only if its not yet assigned
			if (m_gameObjectUIDMap == null)
				SetGameObjectUID();
			
			// Assign UID for all the newly attached components
			Component[]				_componentsList				= CachedGameObject.GetComponents<Component>();
			List<UnityObjectUIDMap>	_componentUIDMapListNew		= new List<UnityObjectUIDMap>();

			foreach (Component _curComponent in _componentsList)
			{
				// If identifier was already alloted then retain it
				int					_curComponentUIDMapIndex	= GetComponentMapIndex(_curComponent);
				UnityObjectUIDMap	_curComponentUIDMap			= _curComponentUIDMapIndex == -1 ? new UnityObjectUIDMap(_curComponent) : m_componentUIDMapList[_curComponentUIDMapIndex];
				
				_componentUIDMapListNew.Add(_curComponentUIDMap);
			} 
			
			// Update component UID list
			m_componentUIDMapList	= _componentUIDMapListNew;
			
			// Check if we need update UID recursively or not
			if (_recursive)
				AssignUIDsRecursively(_reassign: false);
		}
		
		private void UpdateUIDs ()
		{
			UpdateUIDs(_recursive: true);
		}

		/// <summary>
		/// Flushes all the existing UID's and reassigns new UID's to this GameObject and its components.
		/// </summary>
		public void ReassignUIDs (bool _recursive)
		{
			// Reassign new UID for gameObject
			SetGameObjectUID();

			// Reassign new UID for all the component attached to this gameObject
			Component[]				_componentsList				= CachedGameObject.GetComponents<Component>();
			List<UnityObjectUIDMap>	_componentUIDMapListNew		= new List<UnityObjectUIDMap>();

			foreach (Component _curComponent in _componentsList)
			{	
				// Note that this component and gameobject will share same identifier
				if (_curComponent == this)
				{
					_componentUIDMapListNew.Add(new UnityObjectUIDMap(this, m_gameObjectUIDMap.UniqueIdentifier));
					continue;
				}
				
				// If identifier was already alloted then extract serializable property value
				int		_curComponentUIDMapIndex	= GetComponentMapIndex(_curComponent);
				bool	_canSerializeCurComponent	= _curComponentUIDMapIndex == -1 ? true : m_componentUIDMapList[_curComponentUIDMapIndex].CanSerialize;
				
				_componentUIDMapListNew.Add(new UnityObjectUIDMap(_curComponent, _canSerializeCurComponent));
			}
			
			// Update component UID list
			m_componentUIDMapList	= _componentUIDMapListNew;
			
			// Recursively reassign UIDs if required
			if (_recursive)
				AssignUIDsRecursively(_reassign: true);
		}
		
		private void AssignUIDsRecursively (bool _reassign)
		{
			IEnumerator		_enumerator			= CachedTransform.GetEnumerator();
			
			while (_enumerator.MoveNext())
			{
				GameObject	_curChildGO			= ((Transform)_enumerator.Current).gameObject;
				UIDSystem	_curChildUIDSystem	= _curChildGO.AddComponentIfDoesntExist<UIDSystem>();
				
				// Copy parent properties
				_curChildUIDSystem.CopyParentProperties(this);

				// Reassign or update UIDs based on request
				if (_reassign)
					_curChildUIDSystem.ReassignUIDs(true);
				else
					_curChildUIDSystem.UpdateUIDs(true);
			}
		}

		private void CopyParentProperties (UIDSystem _parent)
		{
			this.IsPrefab	= _parent.IsPrefab;
		}

		#endregion

		#region GameObject UID Methods
		
		private void SetGameObjectUID ()
		{
			if (GetComponent<RSManager>() != null)
				SetGameObjectUIDMap(new UnityObjectUIDMap(CachedGameObject, kRSManagerUID, true));
			else
				SetGameObjectUIDMap(new UnityObjectUIDMap(CachedGameObject, true));
		}

		internal void SetGameObjectUID (string _gameObjectUID)
		{
			SetGameObjectUIDMap(new UnityObjectUIDMap(CachedGameObject, _gameObjectUID));
		}

		private void SetGameObjectUIDMap (UnityObjectUIDMap _gameObjectUIDMap)
		{
			// Set gameobject UID value
			m_gameObjectUIDMap						= _gameObjectUIDMap;

			// Use the same UID as identifier for UIDSystem component as well
			UnityObjectUIDMap 	_UIDSystemsUIDMap	= new UnityObjectUIDMap(this, _gameObjectUIDMap.UniqueIdentifier);
			int					_existingValueIndex	= GetComponentMapIndex(this);
		
			if (_existingValueIndex == -1)
				m_componentUIDMapList.Add(_UIDSystemsUIDMap);
			else
				m_componentUIDMapList[_existingValueIndex]	= _UIDSystemsUIDMap;
		}
		
		public string GetGameObjectIdentifier ()
		{
			if (m_gameObjectUIDMap == null)
				Debug.LogException(new Exception(string.Format("[RS] UID value for Gameobject with name {0} is corrupt.", CachedGameObject.name)), CachedGameObject);

			return m_gameObjectUIDMap.UniqueIdentifier;
		}

		#endregion

		#region Component UID Methods

		public Component[] GetSerializableComponents ()
		{
			List<Component>	_components	= new List<Component>();

			foreach (UnityObjectUIDMap _curComponentUIDMap in m_componentUIDMapList)
			{
				if (!_curComponentUIDMap.CanSerialize || _curComponentUIDMap.UnityObject == null)
					continue;
					
				_components.Add((Component)_curComponentUIDMap.UnityObject);
			}

			return _components.ToArray();
		}

		/// <summary>
		/// Generates and assigns a new UID to the given component. 
		/// Operation fails, if UID was already assigned to this component.
		/// </summary>
		/// <param name="_component">Component to which UID needs to be assigned.</param>
		public void AssignUIDToNewComponent (Component _component)
		{
			if (GetComponentMapIndex(_component) == -1)
				m_componentUIDMapList.Add(new UnityObjectUIDMap(_component));
		}

		/// <summary>
		/// Mark the components you would like to serialize while serializing the game object.
		/// </summary>
		/// <param name="_component">Component attached to this gameobject.</param>
		/// <param name="_canSerialize">If set to <c>true</c> then component is serialized.</param>
		public void UpdateCanSerializeComponentStatus (Component _component, bool _canSerialize)
		{
			int 	_componentIndex	= GetComponentMapIndex(_component);
			
			if (_componentIndex == -1)
			{
				Debug.LogError(string.Format("[RS] The operation could not be completed because UID information not found for component type: {0}.", _component.GetType()), _component); 
				return;
			}
			
			m_componentUIDMapList[_componentIndex].CanSerialize	= _canSerialize;
		}

		internal void SetComponentUID (Component _component, string _identifier)
		{
			if (GetComponentMapIndex(_component) == -1)
				m_componentUIDMapList.Add(new UnityObjectUIDMap(_component, _identifier));
		}

		private int GetComponentMapIndex (Component _component)
		{
			return m_componentUIDMapList.FindIndex((UnityObjectUIDMap _curComponentUIDMap) => {
				return(_curComponentUIDMap.UnityObject == _component);
			});
		}
		
		private int GetComponentMapIndex (string _identifier)
		{
			return m_componentUIDMapList.FindIndex((UnityObjectUIDMap _curComponentUIDMap) => {
				return(_curComponentUIDMap.UniqueIdentifier.Equals(_identifier));
			});
		}
		
		public string GetComponentUID (Component _component)
		{
			int 	_componentIndex	= GetComponentMapIndex(_component);
			
			if (_componentIndex == -1)
				Debug.LogException(new Exception(string.Format("[RS] The operation could not be completed because UID information not found for component type: {0}.", _component.GetType())), _component); 

			return m_componentUIDMapList[_componentIndex].UniqueIdentifier;
		}
		
		public new Component GetComponent (string _identifier)
		{
			int 	_componentIndex	= GetComponentMapIndex(_identifier);
			
			if (_componentIndex == -1)
				return null;
			
			return m_componentUIDMapList[_componentIndex].UnityObject as Component;
		}

		#endregion

		#region Deprecated Methods

		[Obsolete("This method is no longer supported. Instead please use UpdateUIDs method.")]
		public void UpdateComponent ()
		{
			UpdateUIDs(true);
		}

		#endregion
	}
}