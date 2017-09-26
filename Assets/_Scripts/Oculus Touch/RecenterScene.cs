using UnityEngine;
using System.Collections;

public class RecenterScene : MonoBehaviour {

    Oculus_TrackedController controller;
    //GrabbableObject obj;
    //float grabThreshold;
    Oculus_HandAnim anim;
    //bool isSelect = false;

    void Awake()
    {
        //DontDestroyOnLoad(transform.parent.parent.parent.parent.gameObject);
        if (GameOptions.initialCameraPos != Vector3.zero)
        {
            Transform cameraTransform = transform.parent.parent.parent.parent;
            cameraTransform.position = GameOptions.initialCameraPos;
            cameraTransform.rotation = GameOptions.initialCameraRot;
        }
        //grabThreshold = 0.5f;
        //obj = null;
        controller = GetComponent<Oculus_TrackedController>();
        anim = GetComponent<Oculus_HandAnim>();
    }

	void OnEnable () {
        anim.SetFlex(0f);
        anim.SetPoint(false);
        anim.SetThumb(false);
        anim.SetPose(0);
        controller.menubuttonPressed += Recenter;
        controller.button1Pressed += NextScene;
	}

    void OnDisable()
    {
        controller.menubuttonPressed -= Recenter;
        controller.button1Pressed -= NextScene;
    }

    void Recenter(object sender, Oculus_ControllerArgs e)
    {
        Transform oculus_transform = transform.parent.parent.parent.parent;
        if(oculus_transform.gameObject.name == "Oculus")
        {
            Quaternion eyeRot = transform.parent.parent.Find("CenterEyeAnchor").rotation;
            eyeRot.x = 0f;
            eyeRot.z = 0f;
            oculus_transform.rotation *= Quaternion.Inverse(eyeRot);

            Vector3 eyePos = transform.parent.parent.Find("CenterEyeAnchor").position;
            oculus_transform.position -= eyePos;
            oculus_transform.position = new Vector3(oculus_transform.position.x, 10f, oculus_transform.position.z);

            GameOptions.initialCameraPos = oculus_transform.position;
            GameOptions.initialCameraRot = oculus_transform.rotation;
            Debug.Log("initialPos = " + GameOptions.initialCameraPos);
        }
    }

    void NextScene(object sender, Oculus_ControllerArgs e)
    {
        UnityEngine.SceneManagement.SceneManager.LoadScene(1);
    }
}
