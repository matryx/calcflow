using UnityEngine;
using System.Collections;

public class MakinVectors : MonoBehaviour {

    public enum GameMode { Create, Edit, Operate };
    public enum OperateMode { addition, crossProduct/*,dotProduct*/}

    public Rigidbody attachpoint;
    public Interaction scene;
    public static GameMode mode = GameMode.Create;
    public static OperateMode operation = OperateMode.addition;
    Arrow currentArrow;
    //public TextMesh options;
    //private TextMesh gamemodetext;
    //private TextMesh operationtext;
    //private TextMesh coordtext;

    private GameObject attachball;

    //public GameObject GRID, SPHERICAL, CYLINDRICAL;

    SteamVR_TrackedObject trackedObj;
    FixedJoint joint;

    /*CGHW*/
    float x, y, z, rho, theta, phi;
    Vector3 vec;
    public static int Cartesian = 0;
    public static int Spherical = 1;
    public static int Cylindrical = 2;
    public static int toggle = Cartesian;
    //static bool changed = true

    void Awake()
    {
        trackedObj = GetComponent<SteamVR_TrackedObject>();
        //GRID.SetActive(true);
        //SPHERICAL.SetActive(false);
        //CYLINDRICAL.SetActive(false);
    }

    void Update()
    {
        var device = SteamVR_Controller.Input((int)trackedObj.index);

        attachball = GetComponentInChildren<AttachmentPoint>().gameObject;
        //Vector4 controllerPosition = new Vector4(attachpoint.transform.position.x, attachpoint.transform.position.y, attachpoint.transform.position.z, 1);
        Vector4 controllerPosition = new Vector4(attachball.transform.position.x, attachball.transform.position.y, attachball.transform.position.z, 1);
        if (mode == GameMode.Create)
        {
            if (currentArrow == null && device.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger))
            {
                Debug.LogWarning("MakinVectors::Update() - Creating vector");
                currentArrow = scene.createVector(controllerPosition);

            }
            else if (currentArrow != null && device.GetTouchUp(SteamVR_Controller.ButtonMask.Trigger))
            {
                currentArrow.finalize();
                currentArrow = null;
            }
            else if (currentArrow != null)
            {

                currentArrow.setPoints(currentArrow.getStart(), controllerPosition);
            }

            if (device.GetTouchDown(SteamVR_Controller.ButtonMask.ApplicationMenu))
            {
                //mode = GameMode.Operate;
                mode = GameMode.Edit;
            }

        }

        else if (mode == GameMode.Edit)
        {
            if (device.GetTouchDown(SteamVR_Controller.ButtonMask.ApplicationMenu))
            {
                mode = GameMode.Operate;
            }
            if(currentArrow == null && device.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger))
            {
                foreach(Arrow v in scene.ourVectors)
                {
                    if((v.getInfo() - controllerPosition).magnitude < 1)
                    {
                        currentArrow = v;
                    }
                }
            }
            else if(currentArrow != null && device.GetTouchUp(SteamVR_Controller.ButtonMask.Trigger))
            {
                currentArrow.FinishEditing();
                currentArrow = null;
            }
            else if(currentArrow != null)
            {
                currentArrow.setPoints(currentArrow.getStart(), controllerPosition);
            }

        }

        else if (mode == GameMode.Operate)
        {
            if (scene.selected.Count < 2 && device.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger))
            {
                foreach (Arrow vector in scene.ourVectors)
                {
                    if ((vector.getInfo() - controllerPosition).magnitude < 1)
                    {
                        scene.selectVector(vector);
                        break;
                    }
                }

            }
            else if (scene.selected.Count == 2 && device.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger))
            {
                if (operation == OperateMode.addition)
                    scene.addSelected();
                if (operation == OperateMode.crossProduct)
                    scene.crossSelected();
                //if (operation == OperateMode.dotProduct)
                //    scene.dotSelected();

            }
            else if (device.GetTouchDown(SteamVR_Controller.ButtonMask.Axis0))
            {
                if (operation == OperateMode.addition)
                    operation = OperateMode.crossProduct;
                else if (operation == OperateMode.crossProduct)
                    operation = OperateMode.addition;
                //else if (operation == OperateMode.dotProduct)
                //    operation = OperateMode.addition;
            }
            if (device.GetTouchDown(SteamVR_Controller.ButtonMask.ApplicationMenu))
            {
                mode = GameMode.Create;
            }

        }
        /*if (device.GetTouchDown(SteamVR_Controller.ButtonMask.Grip))
		{
			scene.getAGrip();
		}
		else if (device.GetTouchUp(SteamVR_Controller.ButtonMask.Grip))
		{
			scene.loseAGrip();
		}*/
        //gamemodetext.text = mode.ToString();
        //if (mode == GameMode.Operate)
        //    operationtext.text = operation.ToString();
        //else
        //    operationtext.text = "";


        //CGHW
        //Change Menu System
        if (device.GetPressDown(SteamVR_Controller.ButtonMask.Touchpad))
        {
            toggle++;
            toggle = toggle % 3;
            /*
            if (toggle == Cartesian)
            {
                // Display "Cartesian"
                coordtext.text = "Cartesian";
                GRID.SetActive(true);
                SPHERICAL.SetActive(false);
                CYLINDRICAL.SetActive(false);
            }
            //else
            //	coordtext.text = "";

            if (toggle == Spherical)
            {
                // Display "Spherical"
                coordtext.text = "Spherical";
                GRID.SetActive(false);
                SPHERICAL.SetActive(true);
                CYLINDRICAL.SetActive(false);
            }
            //else
            //	coordtext.text = "";

            if (toggle == Cylindrical)
            {
                // Display "Cylindrical"
                coordtext.text = "Cylindrical";
                GRID.SetActive(false);
                SPHERICAL.SetActive(false);
                CYLINDRICAL.SetActive(true);
            }
            //else
            //	coordtext.text = "";
            */
        }

    }

        // Use this for initialization
        void Start () {
        /*
        gamemodetext = Instantiate(options);
        gamemodetext.transform.rotation = Quaternion.Euler(0, 90, 0);
        gamemodetext.transform.position = new Vector3(10, 8, 7);
        
        operationtext = Instantiate(options);
        operationtext.transform.rotation = Quaternion.Euler(0, 90, 0);
        operationtext.transform.position = new Vector3(10, 6, 7);

        coordtext = Instantiate(options);
        coordtext.transform.rotation = Quaternion.Euler(0, 90, 0);
        coordtext.transform.position = new Vector3(10, 4, 7);
        coordtext.text = "Cartesian";
        */
	}
	
	// Update is called once per frame

}
