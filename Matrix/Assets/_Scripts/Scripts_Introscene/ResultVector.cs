using UnityEngine;
using System.Collections;

[ExecuteInEditMode]
public class ResultVector : MonoBehaviour {

    public enum ResultType
    {
        ADDITION,
        CROSSPRODUCT
    }

    public ResultType resultType;

    public ConstraintGrabbable oprand1, oprand2;

	// Use this for initialization
	void Start () {
	
	}
	
	// Update is called once per frame
	void Update () {
        switch (resultType)
        {
            case ResultType.ADDITION:
                transform.localPosition = oprand1.lastLocalPos + oprand2.lastLocalPos;
                break;
            case ResultType.CROSSPRODUCT:
                transform.localPosition = Vector3.Cross(oprand1.lastLocalPos, oprand2.lastLocalPos);
                break;
        }
	}
}
