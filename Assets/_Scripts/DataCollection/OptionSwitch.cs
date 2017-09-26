using UnityEngine;
using System.Collections;
using UnityEngine.Analytics;

public class OptionSwitch : MonoBehaviour {

    public bool isEnabled;
    public bool wasEnabled;

    public enum Options
    {
        TextDescriptions,
        SoundFX,
        VoiceOver
    }
    public int index;

    [SerializeField] private ConstraintGrabbable switchBall;

	// Use this for initialization
	void Start () {
        wasEnabled = !isEnabled;
        if (GameOptions.instance.options[index])
        {
            switchBall.lastLocalPos = new Vector3(0.5f, switchBall.lastLocalPos.y, switchBall.lastLocalPos.z);
        }
        else
        {
            switchBall.lastLocalPos = new Vector3(-0.5f, switchBall.lastLocalPos.y, switchBall.lastLocalPos.z);
        }
    }

    /// <summary>
    ///  Function for making analytics possible when the option is switched. 
    /// </summary>
    /// <param name="isEnabled"></param>
    void CreateCustomEvent(bool isEnabled)
    {
        if (isEnabled == false)
        {
            Analytics.CustomEvent(gameObject.name, new System.Collections.Generic.Dictionary<string, object> {
                { "enabled", isEnabled }
            });
        }
    }

    // Update is called once per frame
    void Update () {
	    if (switchBall.lastLocalPos.x < 0f)
        {
            isEnabled = false;
            GameOptions.instance.options[index] = false;

        }
        else
        {
            isEnabled = true;
            GameOptions.instance.options[index] = true;
        }
        if (wasEnabled != isEnabled)
        {
            CreateCustomEvent(isEnabled);
        }
        wasEnabled = isEnabled;
    }
}
