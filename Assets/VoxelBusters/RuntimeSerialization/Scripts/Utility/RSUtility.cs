using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;
using VoxelBusters.Utility;

/// <summary>
/// Utility class which has methods to support serialization for objects created at runtime.
/// </summary>
public class RSUtility
{
    #region Instantiate Methods

    /// <summary>
    /// Creates an exact copy of original game object.
    /// 
    /// When you clone a game object, all child objects and components will also be cloned with their properties set like those of the original object.
    /// Additionally, UIDSystem component will be attached to every game object (only if it doesn't exist). Also new set of unique identifiers are assigned for each game object and its components.
    /// </summary>
    /// <param name="original">The object that you want to clone.</param>
    public static GameObject Instantiate(GameObject original)
    {
        return Instantiate(original, Vector3.zero, Quaternion.identity);
    }

    /// <summary>
    /// Creates an exact copy of original game object.
    /// 
    /// When you clone a game object, all child objects and components will also be cloned with their properties set like those of the original object.
    /// Additionally, UIDSystem component will be attached to every game object (only if it doesn't exist). Also new set of unique identifiers are assigned for each game object and its components.
    /// </summary>
    /// <param name="original">The object that you want to clone.</param>
    /// <param name="position">Position for the new object.</param>
    /// <param name="rotation">Orientation of the new object.</param>
    public static GameObject Instantiate(GameObject original, Vector3 position, Quaternion rotation)
    {
        return Instantiate(original, position, rotation, null);
    }

    /// <summary>
    /// Creates an exact copy of original game object.
    /// 
    /// When you clone a game object, all child objects and components will also be cloned with their properties set like those of the original object.
    /// Additionally, UIDSystem component will be attached to every game object (only if it doesn't exist). Also new set of unique identifiers are assigned for each game object and its components.
    /// </summary>
    /// <param name="original">The object that you want to clone.</param>
    /// <param name="parent">Parent of the new object.</param>
    public static GameObject Instantiate(GameObject original, Transform parent)
    {
        return Instantiate(original, Vector3.zero, Quaternion.identity, parent);
    }


    /// <summary>
    /// Creates an exact copy of original game object.
    /// 
    /// When you clone a game object, all child objects and components will also be cloned with their properties set like those of the original object.
    /// Additionally, UIDSystem component will be attached to every game object (only if it doesn't exist). Also new set of unique identifiers are assigned for each game object and its components.
    /// </summary>
    /// <param name="original">The object that you want to clone.</param>
    /// <param name="position">Position for the new object.</param>
    /// <param name="rotation">Orientation of the new object.</param>
    /// <param name="parent">Parent of the new object.</param>
    public static GameObject Instantiate(GameObject original, Vector3 position, Quaternion rotation, Transform parent)
    {
        if (original == null)
            throw new System.ArgumentNullException("[RS] GameObject argument is null.");

        // Instatiate new object and add UIDSystem component if its not yet added
        GameObject _newClone = (GameObject)Object.Instantiate(original, position, rotation, parent);
        UIDSystem _UIDSystem = _newClone.AddComponentIfDoesntExist<UIDSystem>();

        // Unsetting prefab flag and assign UID's
        _UIDSystem.IsPrefab = false;
        _UIDSystem.ReassignUIDs(_recursive: true);
        Recorder.ResolveSpawns();

        return _newClone;
    }

    #endregion

    #region GameObject Methods

    /// <summary>
    /// Creates a new game object with specified name and attaches the specified components.
    /// 
    /// Additionally, UIDSystem component is attached and along with it, new set of unique identifiers are assigned to the game object as well as its components.
    /// </summary>
    /// <returns>The newly created game object.</returns>
    /// <param name="_name">The string value used for naming this game object.</param>
    /// <param name="_components">Type of components that needs to be attached to this newly created game object.</param>
    public static GameObject CreateGameObject(string _name, params System.Type[] _components)
    {
        // Create gameobject with requested components and also attach UIDSystem
        GameObject _newGameObject = new GameObject(_name, _components);
        UIDSystem _UIDSystem = _newGameObject.AddComponent<UIDSystem>();

        // Assign UID's
        _UIDSystem.UpdateUIDs(_recursive: false);
        Recorder.ResolveSpawns();

        return _newGameObject;
    }

    /// <summary>
    /// Adds a component class to the game object. 
    /// </summary>
    /// <returns>The newly created component instance.</returns>
    /// <param name="_gameObject">The game object to which this new component has to be added.</param>
    /// <typeparam name="T">Type of the component to be added.</typeparam>
    public static T AddComponent<T>(GameObject _gameObject) where T : Component
    {
        if (_gameObject == null)
            throw new System.ArgumentNullException("[RS] GameObject argument is null.");

        // Cache UIDSystem reference
        UIDSystem _UIDSystem = _gameObject.GetComponent<UIDSystem>();

        if (_UIDSystem == null)
            Debug.LogException(new System.NullReferenceException(string.Format("[RS] UIDSystem component not found in GameObject with name {0}.", _gameObject.name)), _gameObject);

        // Add new component and assign UID for this new component
        T _newComponent = _gameObject.AddComponent<T>();

        _UIDSystem.AssignUIDToNewComponent(_newComponent);

        return _newComponent;
    }

    #endregion
}