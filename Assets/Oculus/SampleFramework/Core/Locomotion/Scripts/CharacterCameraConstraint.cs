/************************************************************************************

See SampleFramework license.txt for license terms.  Unless required by applicable law 
or agreed to in writing, the sample code is provided “AS IS” WITHOUT WARRANTIES OR 
CONDITIONS OF ANY KIND, either express or implied.  See the license for specific 
language governing permissions and limitations under the license.

************************************************************************************/

using System;
using UnityEngine;

/// <summary>
/// This component is responsible for moving the character capsule to match the HMD, fading out the camera or blocking movement when 
/// collisions occur, and adjusting the character capsule height to match the HMD's offset from the ground.
/// </summary>
public class CharacterCameraConstraint : MonoBehaviour
{
	/// <summary>
	/// This should be a reference to the OVRCameraRig that is usually a child of the PlayerController.
	/// </summary>
	[Tooltip("This should be a reference to the OVRCameraRig that is usually a child of the PlayerController.")]
	public OVRCameraRig CameraRig;

	/// <summary>
	/// When true, the character capsule won't grow into upwards geo when the player stands up under a low surface.
	/// </summary>
	[Tooltip("When true, the camera will be prevented from passing through collidable geometry. This is usually considered uncomfortable for users.")]
	public bool EnableCollision;
	public LayerMask CollideLayers;

	/// <summary>
	/// This should be set to 1 to make the screen completely fade out when the HMD is inside world geometry. Lesser values can be useful for testing.
	/// </summary>
	[Tooltip("This should be set to 1 to make the screen completely fade out when the HMD is inside world geometry. Lesser values can be useful for testing.")]
	public float MaxFade = 1;

	/// <summary>
	/// This value is used to control how far from the character capsule the HMD must be before the fade to black begins.
	/// </summary>
	[Tooltip("This value is used to control how far from the character capsule the HMD must be before the fade to black begins.")]
	public float FadeMinDistance = 0.25f;

	/// <summary>
	/// If > 0, the capsule will stretch or shrink so that the top of it is at the camera's y location.
	/// Note that if you want the capsule to go a bit higher than the camera you'll need to add your own padding logic.
	/// </summary>
    public float PreferredHeight = 1.0f;

	/// <summary>
	/// This value is used to control how far from the character capsule the HMD must be before the fade to black is complete. 
	/// This should be tuned so that it is fully faded in before the camera will clip geometry that the player should not be able see beyond.
	/// </summary>
	[Tooltip("This value is used to control how far from the character capsule the HMD must be before the fade to black is complete. This should be tuned so that it is fully faded in before the camera will clip geometry that the player should not be able see beyond.")]
	public float FadeMaxDistance = 0.35f;

	private readonly Action _cameraUpdateAction;
	private readonly Action _preCharacterMovementAction;

	private CapsuleCollider _character;
    private SimpleCapsuleWithStickMovement _simplePlayerController;

	CharacterCameraConstraint()
	{
		_cameraUpdateAction = CameraUpdate;
	}

	void Awake ()
	{
		_character = GetComponent<CapsuleCollider>();
		_simplePlayerController = GetComponent<SimpleCapsuleWithStickMovement>();
	}

	private void Start()
	{
	}

	void OnEnable()
	{
        _simplePlayerController.CameraUpdated += _cameraUpdateAction;
	}

	void OnDisable()
	{
        _simplePlayerController.CameraUpdated -= _cameraUpdateAction;
	}

    /// <summary>
    /// This method is the handler for the PlayerController.CameraUpdated event, which is used
    /// to update the character height based on camera position.
    /// </summary>
    private void CameraUpdate()
	{
		// If dynamic height is enabled, try to adjust the controller height to the height of the camera.
		if (PreferredHeight > 0.0f)
		{
            float camHeight = Mathf.Min(CameraRig.centerEyeAnchor.transform.localPosition.y, PreferredHeight);
			float newHeight = camHeight;
			
			// If the new height is less than before, or we don't need to check for collision, just accept the new height.
			if (camHeight <= _character.height || !EnableCollision)
			{
                // we're good, do nothing.
			}
			else
			{
				// Attempt to increase the controller height to the height of the camera.
				// It is important to understand that this will prevent the character from growing into colliding 
				// geometry, and that the camera might go above the character controller. For instance, ducking through
				// a low tunnel and then standing up in the middle would allow the player to see outside the world.
				// The CharacterCameraConstraint is designed to detect this problem and provide feedback to the user,
				// however it is useful to keep the character controller at a size that fits the space because this would allow
				// the player to move to a taller space. If the character controller was simply made as tall as the camera wanted,
				// the player would then be stuck and unable to move at all until the player ducked back down to the 
				// necessary elevation. 
				Vector3 rayStart = _character.transform.position;
				RaycastHit info;
				Vector3 rayEnd = rayStart;
				rayEnd.y += newHeight * 4;
				Debug.DrawLine(rayStart, rayEnd);
				if (Physics.SphereCast(rayStart, _character.radius * 0.2f, Vector3.up, out info, 4.0f,
					CollideLayers, QueryTriggerInteraction.Ignore))
				{
					newHeight = info.distance + _character.radius;
				} // else, no hit, we're fine
			}

			// camHeight/newHeight here is tracking space distance from player's eyes to player's feet.
			// But note that the player controller is centered in the middle of the rigid body.
			// So we move the camera position down by half the player's height to get the eye position to line
			// up with the top of the capsule.
			_character.height = newHeight;
			Vector3 newCamPos = CameraRig.transform.localPosition;
			newCamPos.y = -_character.height * 0.5f;
			CameraRig.transform.localPosition = newCamPos;
		}
	}

}
