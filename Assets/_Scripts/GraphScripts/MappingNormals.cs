using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MappingNormals : MonoBehaviour
{

    public Mapping3D _3dMapper;
    public Mapping1D _1dMapper;

    public Transform uTangent;
    public Transform vTangent;
    public Transform normal;

    // Use this for initialization
    void Start()
    {

    }

    Vector3 DeltaU = new Vector3(.01f, 0f, 0f);
    Vector3 DeltaV = new Vector3(0f, .01f, 0f);


    void HardCodedBullshit()
    {
        ExpressionSet es;
        if(_1dMapper != null)
        {
            es = _1dMapper.calcManager.expressionSet;
        } else
        {
            return;
        }

        Vector3 uvw = _1dMapper.swapYandZ(_1dMapper.uvwSelector.lastLocalPos);

        if (es.expressions["X"].expression == "cos(t^2)" &&
            es.expressions["Y"].expression == "sin(t^2)" &&
            es.expressions["Z"].expression == "0" &&
            es.ranges["t"].Min.expression == "0" &&
            es.ranges["t"].Max.expression == "(2*pi)^(1/2)" &&
            _1dMapper.findUVW(uvw).x == 0)
        {
            uTangent.gameObject.SetActive(false);
        }
    }

    // Update is called once per frame
    void Update()
    {

        Vector3 end;
        Vector3 uTan;
        Vector3 vTan;
        Vector3 start;


        if (_3dMapper != null && _3dMapper.paramSurfaceReady())
        {
            Vector3 correspondingPoint = _3dMapper.CorrespondingPoint.transform.position;
            Vector3 uvw = _3dMapper.swapYandZ(_3dMapper.uvwSelector.lastLocalPos);
            start = _3dMapper.swapYandZ(_3dMapper.findXYZ(_3dMapper.findUVW(uvw)));
            uTan = _3dMapper.swapYandZ(_3dMapper.findXYZ(_3dMapper.findUVW(uvw + DeltaU))) - start;
            vTan = _3dMapper.swapYandZ(_3dMapper.findXYZ(_3dMapper.findUVW(uvw + DeltaV))) - start;
            end = Vector3.Cross(vTan * 10, uTan * 10);

            normal.gameObject.SetActive(end != Vector3.zero);

            normal.position = correspondingPoint;
            normal.LookAt(_3dMapper.CorrespondingPoint.TransformPoint(Scale(end)));
            //normal.localScale = (_3dMapper.CorrespondingPoint.TransformPoint(20*Scale(end)));

            uTangent.gameObject.SetActive(uTan != Vector3.zero);

            uTangent.position = correspondingPoint;
            uTangent.LookAt(_3dMapper.CorrespondingPoint.TransformPoint(Scale(uTan)));

            vTangent.gameObject.SetActive(vTan != Vector3.zero);

            vTangent.position = correspondingPoint;
            vTangent.LookAt(_3dMapper.CorrespondingPoint.TransformPoint(Scale(vTan)));
        }
        if (_1dMapper != null && _1dMapper.paramSurfaceReady())
        {
            Vector3 correspondingPoint = _1dMapper.CorrespondingPoint.transform.position;
            Vector3 uvw = _1dMapper.swapYandZ(_1dMapper.uvwSelector.lastLocalPos);
            start = _1dMapper.swapYandZ(_1dMapper.findXYZ(_1dMapper.findUVW(uvw)));
            uTan = _1dMapper.swapYandZ(_1dMapper.findXYZ(_1dMapper.findUVW(uvw + DeltaU))) - start;
            uTangent.gameObject.SetActive(uTan != Vector3.zero);
            HardCodedBullshit();
            uTangent.position = correspondingPoint;
            uTangent.LookAt(_1dMapper.CorrespondingPoint.TransformPoint(Scale(uTan)));
        }


    }

    float scale = 0.2f;

    Vector3 Scale(Vector3 input)
    {
        //return Vector3.Normalize(input) * scale;
        return input * scale;
    }
}