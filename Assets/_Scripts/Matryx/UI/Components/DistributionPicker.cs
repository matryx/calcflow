using Matryx;
using NanoVRController;
using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DistributionPicker : MonoBehaviour {
    public static Dictionary<MatryxSubmission, float> rewards;

    public MatryxSubmission submission;
    TMPro.TextMeshPro distributionText;
    JoyStickReceiver joystickReceiver;
    bool enabled = false;
    [NonSerialized]
    public float roundBounty = 100;
    

    [NonSerialized]
    public float submissionReward = 0;
    public int submissionPercentage = 0;

	void Start () {
        if (rewards == null)
        {
            rewards = new Dictionary<MatryxSubmission, float>();
        }

        joystickReceiver = GetComponent<JoyStickReceiver>();
        distributionText = transform.parent.Find("Text").GetComponent<TMPro.TextMeshPro>();
	}

    public void Toggle(bool enable)
    {
        if(enabled != enable)
        {
            enabled = enable;
            gameObject.SetActive(enabled);

            if (enabled)
            {
                joystickReceiver.JoyStickTouched += ChangeDistribution;
            }
            else
            {
                joystickReceiver.JoyStickTouched -= ChangeDistribution;
            }
        }
    }

    public void ChangeDistribution(VRController c, ControllerComponentArgs e)
    {
        if (submission != null)
        {
            if (submissionReward <= roundBounty && submissionReward > 0)
            {
                submissionReward += e.x;
                roundBounty -= e.x;
                submissionPercentage = (int)Math.Truncate(submissionReward / roundBounty);
                distributionText.text = submissionPercentage + "%";

                rewards[submission] = submissionReward;
            }

            if (submissionReward > roundBounty)
            {
                submissionReward = roundBounty;
            }
            else if (submissionReward < 0)
            {
                submissionReward = 0;
            }
        }
        else
        {
            throw new System.MissingMemberException("You didn't set the submission for this distribution picker.");
        }
    }

    public void OnDestroy()
    {
        joystickReceiver.JoyStickTouched -= ChangeDistribution;
    }
}
