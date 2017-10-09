using UnityEngine;
using System.Collections;

public class GrabableObject : MonoBehaviour {

    public bool isAttach = false;

    public enum Contraint
    {
        FREE,
        FREE_10x10x10,
        X_CONSTRAINT_10x10,
        Y_CONSTRAINT_10x10,
        MESH_CONSTRAINT,
        X_1D,
        Y_1D,
        Z_1D
    }
    public Contraint constraint;

    public bool isReset = false;

    public bool isGrabbed;

    Vector3 originalPos;
    float counter;

    public Transform indicatorPrefab;
    private Transform indicator;

    private Transform parent;

    [Range(.7f, 1.3f)]
    float indicator_offset = 1.0f;
    bool indicator_offset_incre = true;

    public bool needIndicator = true;

	// Use this for initialization
	void Start () {
        if (transform.parent)
        {
            parent = transform.parent;
        }
        else
        {
            parent = null;
        }

        originalPos = transform.position;
        counter = 100f;

        isGrabbed = false;

        if (needIndicator)
        {
            indicator = Instantiate(indicatorPrefab);
            indicator.SetParent(transform.parent, false);
            indicator.localScale *= transform.parent.localScale.x;
            indicator.localPosition = transform.localPosition + Vector3.up * indicator.localScale.magnitude;
            indicator.rotation = Quaternion.identity;
        }
	}
	
	// Update is called once per frame
	void Update () {
        if (isReset && isGrabbed)
        {
            counter = 100f;
        }
        else if(isReset && !isGrabbed && counter > 0f)
        {
            counter--;
            if(counter <= 0f)
            {
                counter = 100f;
                transform.position = originalPos;
                if (GetComponent<Rigidbody>())
                {
                    GetComponent<Rigidbody>().velocity = Vector3.zero;
                    GetComponent<Rigidbody>().angularVelocity = Vector3.zero;
                }
            }
        }

        if(indicator_offset_incre)
        {
            indicator_offset += 0.005f;
        }
        else
        {
            indicator_offset -= 0.005f;
        }

        if(indicator_offset >= 1.3f)
        {
            indicator_offset_incre = false;
        }
        else if(indicator_offset <= .7f)
        {
            indicator_offset_incre = true;
        }

        if (needIndicator)
        {
            indicator.localPosition = transform.localPosition + Vector3.up * indicator_offset * indicator.localScale.magnitude;
            indicator.Rotate(Vector3.up, 1f);
        }
    }

    public void MoveTowards(Vector3 position)
    {
        isGrabbed = true;
        if (needIndicator)
        {
            indicator.gameObject.SetActive(false);
        }
        switch (constraint)
        {
            case Contraint.FREE:
                transform.position = position;
                break;
            case Contraint.FREE_10x10x10:
                transform.position = position;
                transform.localPosition = new Vector3(
                    (transform.localPosition.x > 10) ? 10 : transform.localPosition.x,
                    (transform.localPosition.y > 10) ? 10 : transform.localPosition.y,
                    (transform.localPosition.z > 10) ? 10 : transform.localPosition.z);
                transform.localPosition = new Vector3(
                    (transform.localPosition.x < -10) ? -10 : transform.localPosition.x,
                    (transform.localPosition.y < -10) ? -10 : transform.localPosition.y,
                    (transform.localPosition.z < -10) ? -10 : transform.localPosition.z);
                break;
            case Contraint.X_CONSTRAINT_10x10:
                position.x = transform.position.x;
                transform.position = position;
                transform.localPosition = new Vector3(
                    (transform.localPosition.x > 10) ? 10 : transform.localPosition.x,
                    (transform.localPosition.y > 10) ? 10 : transform.localPosition.y,
                    (transform.localPosition.z > 10) ? 10 : transform.localPosition.z);
                transform.localPosition = new Vector3(
                    (transform.localPosition.x < -10) ? -10 : transform.localPosition.x,
                    (transform.localPosition.y < -10) ? -10 : transform.localPosition.y,
                    (transform.localPosition.z < -10) ? -10 : transform.localPosition.z);
                break;
            case Contraint.Y_CONSTRAINT_10x10:
                position.y = transform.position.y;
                transform.position = position;
                transform.localPosition = new Vector3(
                    (transform.localPosition.x > 10) ? 10 : transform.localPosition.x,
                    (transform.localPosition.y > 10) ? 10 : transform.localPosition.y,
                    (transform.localPosition.z > 10) ? 10 : transform.localPosition.z);
                transform.localPosition = new Vector3(
                    (transform.localPosition.x < -10) ? -10 : transform.localPosition.x,
                    (transform.localPosition.y < -10) ? -10 : transform.localPosition.y,
                    (transform.localPosition.z < -10) ? -10 : transform.localPosition.z);
                break;
            case Contraint.Z_1D:
                position.x = transform.position.x;
                position.y = transform.position.y;
                transform.position = position;
                transform.localPosition = new Vector3(
                    (transform.localPosition.x > 10) ? 10 : transform.localPosition.x,
                    (transform.localPosition.y > 10) ? 10 : transform.localPosition.y,
                    (transform.localPosition.z > 10) ? 10 : transform.localPosition.z);
                transform.localPosition = new Vector3(
                    (transform.localPosition.x < -10) ? -10 : transform.localPosition.x,
                    (transform.localPosition.y < -10) ? -10 : transform.localPosition.y,
                    (transform.localPosition.z < -10) ? -10 : transform.localPosition.z);
                break;
            case Contraint.X_1D:
                position.z = transform.position.z;
                position.y = transform.position.y;
                transform.position = position;
                transform.localPosition = new Vector3(
                    (transform.localPosition.x > 10) ? 10 : transform.localPosition.x,
                    (transform.localPosition.y > 10) ? 10 : transform.localPosition.y,
                    (transform.localPosition.z > 10) ? 10 : transform.localPosition.z);
                transform.localPosition = new Vector3(
                    (transform.localPosition.x < -10) ? -10 : transform.localPosition.x,
                    (transform.localPosition.y < -10) ? -10 : transform.localPosition.y,
                    (transform.localPosition.z < -10) ? -10 : transform.localPosition.z);
                break;
            case Contraint.Y_1D:
                position.x = transform.position.x;
                position.z = transform.position.z;
                transform.position = position;
                transform.localPosition = new Vector3(
                    (transform.localPosition.x > 10) ? 10 : transform.localPosition.x,
                    (transform.localPosition.y > 10) ? 10 : transform.localPosition.y,
                    (transform.localPosition.z > 10) ? 10 : transform.localPosition.z);
                transform.localPosition = new Vector3(
                    (transform.localPosition.x < -10) ? -10 : transform.localPosition.x,
                    (transform.localPosition.y < -10) ? -10 : transform.localPosition.y,
                    (transform.localPosition.z < -10) ? -10 : transform.localPosition.z);
                break;
        }
        /*
        transform.localPosition = new Vector3(
            (transform.localPosition.x > 10) ? 10 : transform.localPosition.x,
            (transform.localPosition.y > 10) ? 10 : transform.localPosition.y,
            (transform.localPosition.z > 10) ? 10 : transform.localPosition.z);
        transform.localPosition = new Vector3(
            (transform.localPosition.x < -10) ? -10 : transform.localPosition.x,
            (transform.localPosition.y < -10) ? -10 : transform.localPosition.y,
            (transform.localPosition.z < -10) ? -10 : transform.localPosition.z);
            */
    }

    public void Release()
    {
        isGrabbed = false;
        if (isAttach)
        {
            transform.SetParent(parent, true);
        }
        if (needIndicator)
        {
            indicator.gameObject.SetActive(true);
        }
    }
}
