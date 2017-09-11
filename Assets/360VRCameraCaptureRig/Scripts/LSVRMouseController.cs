using UnityEngine;
using System.Collections;
using System.Collections.Generic;

public class LSVRMouseController : MonoBehaviour {


    public float rotationAmount = 12.5f;

    public bool disableMouseRotation = false;

    public bool autoReCenter = true;

    public KeyCode mouseLookKey = KeyCode.Mouse1;

    public float dampingTime = 0.2f;

    protected Camera DebugCamera = null;
    private Vector3 m_TargetAngles;
    private Vector3 m_FollowAngles;
    private Vector3 m_FollowVelocity;
    private Quaternion m_OriginalRotation;

    void Awake()
    {
        DebugCamera = this.GetComponent<Camera>();
    }

    void Start ()
    {
        m_OriginalRotation = transform.localRotation;
    }

    private void Update()
    {
        // we make initial calculations from the original local rotation
        transform.localRotation = m_OriginalRotation;

        // read input from mouse
        float inputH = 0f;
        float inputV = 0f;

        // only look if mouseLookKey is pressed or if no mouseLookKey is set
        if (Input.GetKey(mouseLookKey) == false && mouseLookKey != KeyCode.None)
        {
            // auto recenter when action button is released
            if(autoReCenter)
            {
                m_TargetAngles.y = Mathf.Lerp(-360.0f * 0.5f, 360.0f * 0.5f, inputH * .5f + .5f);
                m_TargetAngles.x = Mathf.Lerp(-360.0f * 0.5f, 360.0f * 0.5f, inputV * .5f + .5f);
            }
        }
        else
        {
            inputH = Input.GetAxis("Mouse X");
            inputV = Input.GetAxis("Mouse Y");
        }


        // wrap values to avoid springing quickly the wrong way from positive to negative
        if (m_TargetAngles.y > 180)
        {
            m_TargetAngles.y -= 360;
            m_FollowAngles.y -= 360;
        }
        if (m_TargetAngles.x > 180)
        {
            m_TargetAngles.x -= 360;
            m_FollowAngles.x -= 360;
        }
        if (m_TargetAngles.y < -180)
        {
            m_TargetAngles.y += 360;
            m_FollowAngles.y += 360;
        }
        if (m_TargetAngles.x < -180)
        {
            m_TargetAngles.x += 360;
            m_FollowAngles.x += 360;
        }

        // with mouse input, we have direct control with no springback required.
        m_TargetAngles.y += inputH * rotationAmount;
        m_TargetAngles.x += inputV * rotationAmount;

        // clamp values to allowed range
        m_TargetAngles.y = Mathf.Clamp(m_TargetAngles.y, -360.0f * 0.5f, 360.0f * 0.5f);
        m_TargetAngles.x = Mathf.Clamp(m_TargetAngles.x, -360.0f * 0.5f, 360.0f * 0.5f);


        // smoothly interpolate current values to target angles
        m_FollowAngles = Vector3.SmoothDamp(m_FollowAngles, m_TargetAngles, ref m_FollowVelocity, dampingTime);

        // update the actual gameobject's rotation
        transform.localRotation = m_OriginalRotation * Quaternion.Euler(-m_FollowAngles.x, m_FollowAngles.y, 0);
    }


}


