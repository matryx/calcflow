using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility
{
	public static class GameObjectExtensions  
	{
		#region Create Child Methods

		public static GameObject AddChild (this GameObject _parentGO, string _childName)
		{
			GameObject _childGO				= new GameObject(_childName);

			return _parentGO.AddChild(_childGO, Vector3.zero, Quaternion.identity, Vector3.zero);
		}

		public static GameObject AddChild (this GameObject _parentGO, string _childName, Vector3 _localPosition, Quaternion _localRotation, Vector3 _localScale)
		{
			GameObject _childGO				= new GameObject(_childName);

			return _parentGO.AddChild(_childGO, _localPosition, _localRotation, _localScale);
		}

		public static GameObject AddChild (this GameObject _parentGO, GameObject _childGO)
		{
			return _parentGO.AddChild(_childGO, Vector3.zero, Quaternion.identity, Vector3.zero);
		}

		public static GameObject AddChild (this GameObject _parentGO, GameObject _childGO, Vector3 _localPosition, Quaternion _localRotation, Vector3 _localScale)
		{
			// Parent and child transform
			Transform _parentTransform		= _parentGO.transform;
			Transform _childTransform		= _childGO.transform;

			// Set as parent
			_childTransform.parent			= _parentTransform;

			// Set local position, rotation, scale
			_childTransform.localPosition	= _localPosition;
			_childTransform.localRotation	= _localRotation;
			_childTransform.localScale		= _localScale;

			return _childGO;
		}

		#endregion

		#region Property Methods

		public static string GetPath (this GameObject _gameObject)
		{
			if (_gameObject == null)
				return null;

			return _gameObject.transform.GetPath();
		}

		#endregion

		#region Component Methods

		public static T AddInvisibleComponent<T> (this GameObject _gameObject) where T : MonoBehaviour
		{
			T _newComponent			= _gameObject.AddComponent<T>();
			_newComponent.hideFlags	= HideFlags.HideInInspector;

			return _newComponent;
		}

		#endregion

		#region Create Methods

		public static GameObject CreateGameObjectAtPath (string _path)
		{
			string _formattedPath		= _path.Trim('/');
			string[] _pathComponents	= _formattedPath.Split('/');
			int _count					= _pathComponents.Length;
			int _index					= 0;

			return CreateGameObject(_pathComponents, ref _index, _count);
		}

		private static GameObject CreateGameObject (string[] _pathComponents, ref int _index, int _count, Transform _parentTransform = null)
		{
			// Terminating conditions
			if (_count == 0)
				return null;

			if (_index >= _count)
				return _parentTransform.gameObject;

			// Initial condition to find root object
			if (_index == 0)
			{
				string _gameObjectName		= _pathComponents[0];
				GameObject _parentGO		= GameObject.Find("/" + _gameObjectName);

				if (_parentGO == null)
					_parentGO				= new GameObject(_gameObjectName);

				// Move to next entry
				_index++;
				return CreateGameObject(_pathComponents, ref _index, _count, _parentGO.transform);
			}
		
			// Check for immediate child 
			Transform _childTransform	= _parentTransform.Find(_pathComponents[_index]);
			
			if (_childTransform != null)
			{
				_index++;
				return CreateGameObject(_pathComponents, ref _index, _count, _childTransform);
			}

			// Child transform couldnt be found, it implies that we need to create gameobjects for remaining path components
			GameObject _gameObject		= _parentTransform.gameObject;

			while (_index < _count)
			{
				string _childGOName		= _pathComponents[_index++];
				_gameObject				= _gameObject.AddChild(_childGOName);
			}

			return _gameObject;
		}

		#endregion
	}
}