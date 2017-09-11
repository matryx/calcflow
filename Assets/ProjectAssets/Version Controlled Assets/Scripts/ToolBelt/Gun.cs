using UnityEngine;
using System.Collections;
using NanoVRController;

public class Gun : Tool {
    GameObject ball;
    Rigidbody rb;
    float ballsize = 0;
    // Use this for initialization

    public override void triggerPressOperation(VRController c, ControllerComponentArgs e)
    {
        base.triggerPressOperation(c, e);

        switch (currMode)
        {
            case 0:
                ball = GameObject.CreatePrimitive(PrimitiveType.Sphere);
                ball.transform.localScale = new Vector3(.01f, .01f, .01f);
                ball.transform.position = tip.position;
                rb = ball.AddComponent<Rigidbody>();
                rb.interpolation = RigidbodyInterpolation.Interpolate;
                rb.collisionDetectionMode = CollisionDetectionMode.Continuous;
                rb.velocity = transform.forward * 10;
                rb.useGravity = false;
                break;
            case 1:
                break;
            case 2:
                ball = GameObject.CreatePrimitive(PrimitiveType.Sphere);
                ball.transform.localScale = new Vector3(.01f, .01f, .01f);
                ball.transform.position = tip.position;
                ball.transform.SetParent(tip, true);
                ballsize = .01f;
                i = 0;
                break;
        }
    }

    public override void triggerHoldOperation(VRController c, ControllerComponentArgs e)
    {
        base.triggerHoldOperation(c, e);
        switch (currMode)
        {
            case 0:
                break;
            case 1:
                ball = GameObject.CreatePrimitive(PrimitiveType.Sphere);
                ball.transform.localScale = new Vector3(.01f, .01f, .01f);
                ball.transform.position = tip.position;
                rb = ball.AddComponent<Rigidbody>();
                rb.interpolation = RigidbodyInterpolation.Interpolate;
                rb.collisionDetectionMode = CollisionDetectionMode.Continuous;
                rb.velocity = transform.forward * 10;
                rb.useGravity = false;
                break;
            case 2:
                ballsize += 1/(ballsize/.0001f);
                ball.transform.localScale += new Vector3(ballsize, ballsize, ballsize);
                break;
        }
    }
    int i = 0;
    public override void triggerReleaseOperation(VRController c, ControllerComponentArgs e)
    {
        base.triggerReleaseOperation(c, e);

        switch (currMode)
        {
            case 0:
                break;
            case 1:
                break;
            case 2:
                if (ball != null)
                {
                    print(i++);
                    ball.name = "CurveBall";
                    ball.transform.SetParent(null, true);
                    ball.AddComponent<Rigidbody>();
                    rb = ball.GetComponent<Rigidbody>();

                    rb.interpolation = RigidbodyInterpolation.Interpolate;
                    rb.mass = ballsize / .01f;
                    rb.collisionDetectionMode = CollisionDetectionMode.Continuous;
                    rb.velocity = transform.forward * 10;
                    rb.useGravity = false;
                }
                break;
        }
    }

    protected override void triggerNotHeld()
    {
        base.triggerNotHeld();
        if (rb && rb.name == "CurveBall")
        {
            rb.velocity = transform.forward * 10;
        }

    }

    GameObject barrel = null;
    Color[] colorOptions = { Color.white, Color.red, Color.black };

    protected override void switchModeUp()
    {
        base.switchModeUp();

        if (!barrel)
        {
             barrel = gameObject.transform.Find("Barrel").gameObject;
        }
        barrel.GetComponent<Renderer>().material.color = colorOptions[currMode];


    }

    protected override void switchModeDown()
    {
        base.switchModeDown();
        if (!barrel)
        {
             barrel = gameObject.transform.Find("Barrel").gameObject;
        }
        barrel.GetComponent<Renderer>().material.color = colorOptions[currMode];
    }


}
