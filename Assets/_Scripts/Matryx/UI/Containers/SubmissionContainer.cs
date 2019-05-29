using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using Matryx;

public class SubmissionContainer : FlexHoverTipper
{
    public MatryxSubmission submission { get; set; }
    public DistributionPicker distributionPicker;

    public override bool shouldShowTippy()
    {
        return text.textInfo.pageCount > 1;
    }

    private new void Start()
    {
        base.Start();

        distributionPicker = transform.Find("Distribution/Body").GetComponent<DistributionPicker>();

        fontSize = 1.1f;
        alignment = TMPro.TextAlignmentOptions.Left;
        var body = transform.Find("Body");
        size = new Vector3(body.lossyScale.x, body.lossyScale.y, body.lossyScale.z);
        lifetime = 120f;
        location = gameObject.transform;
        offset = new Vector3(0f, -0.9f, -0.02f);
        fadeInDuration = fadeOutDuration = 0.2f;
        movementMode = Tippy.MovementMode.Exact;
    }
}
