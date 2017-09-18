using UnityEngine;
using System.Collections;

public class GameOptions : MonoBehaviour {

    public static Vector3 initialCameraPos;
    public static Quaternion initialCameraRot;
    public static int initializedControllerCount = 0;
    public static GameOptions instance = null;

    public enum OPTIONS
    {
        TextDescriptions = 0,
        SoundFX = 1,
        VoiceOver = 2
    }

    public bool[] options;

    [SerializeField]
    private OptionSwitch descriptionSwitch;

	// Use this for initialization
	void Awake () {
        if(instance == null)
        {
            instance = this;
        }
        else if(instance != null && instance != this)
        {
            //Debug.Log("destroyed");
            Destroy(gameObject);
        }

        DontDestroyOnLoad(gameObject);

        options = new bool[] { true, true, true };
	}

    //void Start()
    //{
    //    Oculus.Platform.Entitlements.IsUserEntitledToApplication().OnComplete(
    //            (Oculus.Platform.Message msg) =>
    //            {
    //                if (msg.IsError)
    //                {
    //                    // User is NOT entitled.
    //                    //showMessageThatTheUserDoesntOwnThis();
    //                    Debug.Log("Entitlement check failed. Make sure that you own a legit copy of this software.");
    //                    Application.Quit();
    //                }
    //                else
    //                {
    //                    // User IS entitled
    //                    //proceedAsNormal();
    //                    Debug.Log("Entitlement check passed.");
    //                }
    //            }
    //        );
    //}
}
