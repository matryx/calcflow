using Matryx;
using NanoVRController;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Numerics;
using UnityEngine;

public class DistributionPicker : MonoBehaviour {
    public static Dictionary<MatryxSubmission, decimal> rewards = new Dictionary<MatryxSubmission, decimal>();
    public static List<MatryxSubmission> submissions = new List<MatryxSubmission>();

    [NonSerialized]
    public static decimal DistributionSum = 0;

    [SerializeField]
    SubmissionContainer submissionContainer;
    TMPro.TextMeshPro distributionText;
    JoyStickReceiver joystickReceiver;
    bool enabled = false;

	void OnEnable () {
        if(submissionContainer.submission != null)
        {
            AddToDistribution(); 
        }
	}

    private void OnDisable()
    {
        if (submissionContainer.submission != null)
        {
            RemoveFromDistribution();
        }
    }

    bool initialized = false;
    public void Initialize()
    {
        if (!initialized)
        {
            joystickReceiver = GetComponent<JoyStickReceiver>();
            distributionText = transform.parent.Find("Text").GetComponent<TMPro.TextMeshPro>();

            initialized = true;
        }
    }

    public void AddToDistribution()
    {
        var count = submissions.Count == 0 ? 1 : submissions.Count;
        var weightedInitial = 100 / (decimal)count;
        submissions.Add(submissionContainer.submission);
        rewards.Add(submissionContainer.submission, weightedInitial);
        DistributionSum += weightedInitial;
        BalanceDistribution();
    }

    public void RemoveFromDistribution()
    {
        submissions.Remove(submissionContainer.submission);
        DistributionSum -= rewards[submissionContainer.submission];
        rewards.Remove(submissionContainer.submission);
        BalanceDistribution(false);
    }

    public void ShiftReward(decimal amount)
    {
        if (rewards[submissionContainer.submission]+amount < 0)
        {
            rewards[submissionContainer.submission] = 0;
            return;
        }

        if (rewards.Count < 2)
        {
            return;
        }

        DistributionSum += amount;
        rewards[submissionContainer.submission] += amount;
    }

    public void BalanceDistribution(bool onlyAtThreshold = true)
    {
        if (onlyAtThreshold && DistributionSum > 100 ||
            !onlyAtThreshold)
        {
            decimal sum = 0;
            foreach (MatryxSubmission submission in submissions)
            {
                rewards[submission] = 100 * (rewards[submission] / DistributionSum);
                sum += rewards[submission];
            }

            DistributionSum = sum;
        }
    }

    public void UpdateUI()
    {
        distributionText.text = Math.Truncate(rewards[submissionContainer.submission]) + "%";
    }

    public void Toggle(bool enbl)
    {
        Initialize();

        if (enabled != enbl)
        {
            enabled = enbl;
            transform.parent.gameObject.SetActive(enabled);
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
        if (submissionContainer.submission != null)
        {
            ShiftReward((decimal)e.x);
            BalanceDistribution();
        }
        else
        {
            throw new System.MissingMemberException("You didn't set the submission for this distribution picker.");
        }
    }

    public void Update()
    {
        if(enabled)
        {
            UpdateUI();
        }
    }

    public void OnDestroy()
    {
        joystickReceiver.JoyStickTouched -= ChangeDistribution;
    }
}
