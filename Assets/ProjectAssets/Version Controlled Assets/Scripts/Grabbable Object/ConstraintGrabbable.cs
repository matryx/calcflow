using UnityEngine;
using System.Collections;
using NanoVRController;


public class ConstraintGrabbable : GrabbableObject {

    [SerializeField]
    float x_min=0, x_max=0, y_min=0, y_max=0, z_min=0, z_max=0;

    public Vector3 lastLocalPos = Vector3.zero;
    Quaternion lastLocalRotation;
	Vector3 lastScale;

    public Transform sliderOrigin;

    protected override void Awake()
    {
        base.Awake();
        initialize();		
    }

    public void SetXRange(float min, float max)
    {
        x_min = min; x_max = max;
    }
    public void SetYRange(float min, float max)
    {
        y_min = min; y_max = max;
    }
    public void SetZRange(float min, float max)
    {
        z_min = min; z_max = max;
    }

    void initialize()
    {
        transform.SetParent(sliderOrigin);
        lastLocalPos = transform.localPosition;
        lastScale = transform.localScale;
        lastLocalRotation = transform.localRotation;
        transform.parent = null;
    }

    protected override void LateUpdate()
    {
        base.LateUpdate();
        ConstrainManagement();
    }


	private void ConstrainManagement()
    {
        Transform parent = transform.parent;
        transform.SetParent(sliderOrigin);
        if (!IsGrabbed)
        {
            MoveWithOrigin();
        }
        MoveTowards();

        lastLocalPos = transform.localPosition;
        lastScale = transform.localScale;
        lastLocalRotation = transform.localRotation;

        transform.parent = parent;
	}

	private void MoveWithOrigin()
    {
        transform.localPosition = lastLocalPos;
        transform.localScale = lastScale;
        transform.localRotation = lastLocalRotation;

		return;
    }

    private void MoveTowards()
    {
        Vector3 localpos = transform.localPosition;
        localpos.x = (localpos.x < x_min) ? x_min : localpos.x;
        localpos.x = (localpos.x > x_max) ? x_max : localpos.x;
        localpos.y = (localpos.y < y_min) ? y_min : localpos.y;
        localpos.y = (localpos.y > y_max) ? y_max : localpos.y;
        localpos.z = (localpos.z < z_min) ? z_min : localpos.z;
        localpos.z = (localpos.z > z_max) ? z_max : localpos.z;
        transform.localPosition = localpos;
    }

}
