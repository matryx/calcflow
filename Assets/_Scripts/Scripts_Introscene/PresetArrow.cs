using UnityEngine;
using System;
using System.Collections;

[ExecuteInEditMode]
public class PresetArrow : MonoBehaviour {
	public Transform origin;
    public ConstraintGrabbable constraintGrabbable;
    public string vecName;
	// Use this for initialization
	void Start () {
	
	}
	
	// Update is called once per frame
	void Update () {

        if (constraintGrabbable != null)
        {
            if (constraintGrabbable.IsGrabbed == false)
            {
                constraintGrabbable.lastLocalPos = new Vector3(Mathf.Round(constraintGrabbable.lastLocalPos.x),
                    Mathf.Round(constraintGrabbable.lastLocalPos.y), Mathf.Round(constraintGrabbable.lastLocalPos.z));
            }
        }
        Vector3 position = transform.position;

        transform.rotation = Quaternion.LookRotation(origin.position-position);

        LineRenderer line = GetComponent<LineRenderer>();
        line.SetPosition(0, transform.position);
        line.SetPosition(1, origin.position);

        TextMesh text = GetComponentInChildren<TextMesh>();
        if (constraintGrabbable)
        {
            text.text = vecName + "<" + String.Format("{0:F3}", constraintGrabbable.lastLocalPos.x) + ", " +
                String.Format("{0:F3}", constraintGrabbable.lastLocalPos.z) + ", " +
                String.Format("{0:F3}", constraintGrabbable.lastLocalPos.y) + ">";
        } else
        {
            text.text = vecName + "<" + String.Format("{0:F3}", transform.localPosition.x) + ", " +
                String.Format("{0:F3}", transform.localPosition.z) + ", " +
                String.Format("{0:F3}", transform.localPosition.y) + ">";
        }

        text.transform.rotation = Quaternion.LookRotation(transform.position - Camera.main.transform.position);
	}
}
