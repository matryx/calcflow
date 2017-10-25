using UnityEngine;
using System.Collections;
using System;

#if UNITY_EDITOR
using UnityEditor;
#endif

namespace VoxelBusters.RuntimeSerialization
{
	using Internal;

	public partial class UIDSystem : MonoBehaviour 
	{
		[Serializable]
		private class UnityObjectUIDMap
		{
			#region Fields
			
			[SerializeField]
			private			UnityEngine.Object		m_unityObject;
			[SerializeField]
			private			string					m_uniqueIdentifier;
			[SerializeField]
			private			bool					m_canSerialize		= true;

			#endregion
		
			#region Properties

			public UnityEngine.Object UnityObject
			{
				get
				{
					return m_unityObject;
				}

				private set
				{
					m_unityObject	= value;
				}
			}
			
			public string UniqueIdentifier
			{
				get
				{
					return m_uniqueIdentifier;
				}
				
				private set
				{
					m_uniqueIdentifier	= value;
				}
			}
			
			public bool CanSerialize
			{
				get
				{
					return m_canSerialize;
				}
				
				set
				{
					m_canSerialize	= value;
				}
			}
			
			#endregion

			#region Constructors

			public UnityObjectUIDMap()
			{}
		
			public UnityObjectUIDMap(UnityEngine.Object _object, bool _canSerialize = true) : this(_object, ShortGUID.Create(), _canSerialize)
			{}

			public UnityObjectUIDMap(UnityEngine.Object _object, string _uniqueIdentifier, bool _canSerialize = true)
			{
				// Initialise
				this.UnityObject		= _object;
				this.UniqueIdentifier	= _uniqueIdentifier;
				this.CanSerialize		= _canSerialize;
			}

			#endregion
		}

#if UNITY_EDITOR
		[CustomPropertyDrawer(typeof(UnityObjectUIDMap))]
		public class UnityObjectUIDMapDrawer : PropertyDrawer 
		{
			#region Drawer Methods
			
			public override float GetPropertyHeight(SerializedProperty _property, GUIContent _label) 
			{
				return EditorGUIUtility.singleLineHeight;
			}
			
			public override void OnGUI(Rect _position, SerializedProperty _property, GUIContent _label) 
			{
				// Cache info
				SerializedProperty	_objectProperty			= _property.FindPropertyRelative("m_unityObject");
				SerializedProperty	_identifierProperty		= _property.FindPropertyRelative("m_uniqueIdentifier");
				SerializedProperty	_canSerializeProperty	= _property.FindPropertyRelative("m_canSerialize");
				UnityEngine.Object	_objectValue			= _objectProperty.objectReferenceValue;
				bool				_isGameObject			= false;
				bool				_isUIDSystemComponent	= false;
				bool				_isTypeSupported		= false;

				if(_objectValue != null)
				{
					Type			_objectType				= _objectValue.GetType();
					_isGameObject							= _objectType.IsAssignableFrom(typeof(GameObject));
					_isUIDSystemComponent					= _objectType.IsAssignableFrom(typeof(UIDSystem));
					_isTypeSupported						= SerializationTypeUtil.IsRuntimeSerializableObject(_objectType);
				}

				// Start drawing property
				EditorGUI.BeginProperty(_position, _label, _property);
				
				// Draw prefix label
				_position				= EditorGUI.PrefixLabel(_position, GUIUtility.GetControlID(FocusType.Passive), _label);

				// Don't make child fields be indented
				int  indent 			= EditorGUI.indentLevel;
				EditorGUI.indentLevel 	= 0;

				// Calculate rectangle
				Rect	_objectRect, _identifierRect;

				_objectRect				= new Rect(_position.x, _position.y, 160f, _position.height);
				
				if (_isGameObject)
				{
					_identifierRect		= new Rect(_position.x + 165f, _position.y, _position.width - 165f, _position.height);
				}
				else
				{
					_identifierRect		= new Rect(_position.x + 165f, _position.y, _position.width - 190f, _position.height);
				}

				// Using color feedback to let user know if type is supported or not
				Color	_GUIColorOld	= GUI.color;
				GUI.color				= _isTypeSupported ? Color.green : Color.red;

				// Draw non editable properties object and associated identifier 
				GUI.enabled 			= false;
				
				EditorGUI.PropertyField(_objectRect, _objectProperty, GUIContent.none);
				EditorGUI.PropertyField(_identifierRect, _identifierProperty, GUIContent.none);

				// Reverting back to original value
				GUI.color				= _GUIColorOld;
				GUI.enabled				= true;

				if (!_isGameObject)
				{
					Rect	_canSerializeRect	= new Rect(_identifierRect.xMax + 5f, _position.y, 25f, _position.height);

					// Modifying can_serialize property is allowed only for all components except UIDSystem
					GUI.enabled					= !_isUIDSystemComponent;

					EditorGUI.PropertyField(_canSerializeRect, _canSerializeProperty, GUIContent.none);

					// Reverting back to default value
					GUI.enabled					= true;
				}

				// Reverting back to original value
				EditorGUI.indentLevel 	= indent;
				
				EditorGUI.EndProperty();
			}
			
			#endregion
		}
#endif
	}
}