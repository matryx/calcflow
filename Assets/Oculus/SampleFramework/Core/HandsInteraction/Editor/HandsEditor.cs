/************************************************************************************

Copyright (c) Facebook Technologies, LLC and its affiliates. All rights reserved.

See SampleFramework license.txt for license terms.  Unless required by applicable law
or agreed to in writing, the sample code is provided “AS IS” WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied.  See the license for specific
language governing permissions and limitations under the license.

************************************************************************************/

using UnityEditor;

namespace OculusSampleFramework
{
	/// <summary>
	/// Custom editor to display _leftHand and _rightHand only if 
	/// handPrefab isn't set.
	/// </summary>
	[CustomEditor(typeof(Hands))]
	[CanEditMultipleObjects]
	public class HandsEditor : Editor
	{
		SerializedProperty _handPrefab;
		SerializedProperty _leftHand;
		SerializedProperty _rightHand;
		SerializedProperty _visualMode;

		private void OnEnable()
		{
			_handPrefab = serializedObject.FindProperty("_handPrefab");
			_leftHand = serializedObject.FindProperty("_leftHand");
			_rightHand = serializedObject.FindProperty("_rightHand");
			_visualMode = serializedObject.FindProperty("VisualMode");
		}

		public override void OnInspectorGUI()
		{
			serializedObject.Update();
			EditorGUILayout.PropertyField(_handPrefab);
			EditorGUILayout.PropertyField(_visualMode);
			if (_handPrefab.objectReferenceValue == null)
			{
				EditorGUILayout.PropertyField(_leftHand);
				EditorGUILayout.PropertyField(_rightHand);
			}
			serializedObject.ApplyModifiedProperties();
		}
	}
}
